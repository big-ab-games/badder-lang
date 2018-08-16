use super::*;
use single_value_channel;
use std::sync::mpsc;
use std::sync::mpsc::{RecvTimeoutError, TryRecvError};
use std::time::*;
use std::{thread, u32, u64};

const STACK_SIZE: usize = 8 * 1024 * 1024;
const BADDER_STACK_LEN: usize = 200;

#[derive(Clone, Debug)]
pub struct Phase {
    pub id: u64,
    pub time: Instant,
    pub src: SourceRef,
    /// most recent function call ref relavant to this Ast, empty => top level code
    pub called_from: Vec<SourceRef>,
    kind: PhaseKind,
    unpaused: bool,
    pub stack: Arc<Vec<FxIndexMap<Token, FrameData>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PhaseKind {
    FunctionCall,
    Assignment,
    Other,
}

impl PhaseKind {
    fn from(ast: &Ast) -> PhaseKind {
        match *ast {
            Ast::Call(..) => PhaseKind::FunctionCall,
            Ast::Assign(..) | Ast::AssignFun(..) | Ast::AssignSeq(..) => PhaseKind::Assignment,

            _ => PhaseKind::Other,
        }
    }
}

#[derive(Debug, Clone)]
enum OverseerUpdate {
    Phase(Phase),
    FinishedFunCall(Token),
}

struct ControllerOverseer {
    next_id: u64,
    pause_time: single_value_channel::Receiver<Duration>,
    to_controller: mpsc::Sender<OverseerUpdate>,
    from_controller: mpsc::Receiver<Result<u64, ()>>,
    external_function_ids: Vec<Token>,
    external_function_call: mpsc::Sender<ExternalCall>,
    external_function_answer: mpsc::Receiver<Result<Int, String>>,
    last_stack_copy: Option<Arc<Vec<FxIndexMap<Token, FrameData>>>>,
}

fn interested_in(ast: &Ast) -> bool {
    use Ast::*;
    match *ast {
        LinePair(..) | Line(..) | Empty(..) | Num(..) | ReferSeq(..) | ReferSeqIndex(..)
        | Refer(..) => false,
        _ => true,
    }
}

impl ControllerOverseer {
    fn replace_last_stack(
        &mut self,
        stack: &[FxIndexMap<Token, FrameData>],
    ) -> Arc<Vec<FxIndexMap<Token, FrameData>>> {
        let last: Arc<Vec<_>> = Arc::new(stack.into());
        self.last_stack_copy = Some(Arc::clone(&last));
        last
    }
}

impl Overseer for ControllerOverseer {
    fn oversee(
        &mut self,
        stack: &[FxIndexMap<Token, FrameData>], // Cow?
        ast: &Ast,
        _current_scope: usize,
        _stack_key: StackKey,
    ) -> Result<(), ()> {
        if !interested_in(ast) {
            return Ok(());
        }

        let id = self.next_id;
        let send_time = Instant::now();
        self.next_id = id.overflowing_add(1).0;

        let stack = {
            // kerfuffle to avoid cloning the stack when it hasn't changed
            if let Some(last) = self.last_stack_copy.take() {
                if last.as_slice() == stack {
                    self.last_stack_copy = Some(Arc::clone(&last));
                    last
                } else {
                    self.replace_last_stack(stack)
                }
            } else {
                self.replace_last_stack(stack)
            }
        };

        trace!("ControllerOverseer sending: {:?} {:?}", ast.src(), ast);
        self.to_controller
            .send(OverseerUpdate::Phase(Phase {
                id,
                src: ast.src(),
                called_from: Vec::new(), // unknown
                kind: PhaseKind::from(ast),
                unpaused: false,
                time: send_time,
                stack,
            }))
            .expect("send");

        let mut recv = self.from_controller.try_recv();
        while recv != Err(TryRecvError::Empty) {
            match recv {
                Ok(Ok(i)) => if i == id {
                    return Ok(());
                },
                Ok(Err(_)) | Err(TryRecvError::Disconnected) => {
                    debug!("ControllerOverseer cancelling: {:?} {:?}", ast.src(), ast);
                    return Err(());
                }
                _ => (),
            }
            recv = self.from_controller.try_recv();
        }

        let pause_time = *self.pause_time.latest();
        if send_time.elapsed() >= pause_time {
            Ok(())
        } else {
            // block until result received
            debug!("ControllerOverseer waiting: {:?} {:?}", ast.src(), ast);

            let mut elapsed = send_time.elapsed();
            while elapsed < pause_time {
                match self.from_controller.recv_timeout(pause_time - elapsed) {
                    Ok(Ok(i)) => if i == id {
                        return Ok(());
                    },
                    Ok(Err(_)) | Err(RecvTimeoutError::Disconnected) => {
                        debug!("ControllerOverseer cancelling: {:?} {:?}", ast.src(), ast);
                        return Err(());
                    }
                    _ => (),
                };

                elapsed = send_time.elapsed();
            }
            Ok(())
        }
    }

    fn oversee_after(&mut self, _stack: &[FxIndexMap<Token, FrameData>], ast: &Ast) {
        if let Ast::Call(ref id, ..) = *ast {
            let _ = self
                .to_controller
                .send(OverseerUpdate::FinishedFunCall(id.clone()));
        }
    }

    fn external_function_signatures(&self) -> &[Token] {
        &self.external_function_ids
    }

    fn call_external_function(&mut self, id: Token, args: Vec<Int>) -> Result<Int, String> {
        debug!(
            "ControllerOverseer awaiting answer: {:?}, args {:?}",
            id, args
        );
        self.external_function_call
            .send(ExternalCall { id, args })
            .expect("send");

        // block until answer received
        match self.external_function_answer.recv() {
            Ok(result) => result,
            Err(err) => {
                debug!("ControllerOverseer cancelling: {:?}", err);
                Err("cancelled".into())
            }
        }
    }
}

#[derive(Debug)]
pub struct Controller {
    pause_time: Duration,
    current_phase: Option<Phase>,
    fun_call_history: Vec<SourceRef>,
    result: Option<Res<Int>>,
    current_external_call: Option<ExternalCall>,
    external_function_ids: Vec<Token>,
    run_stats: RunStats,

    /// #cancel() has been called this run
    cancelled: bool,

    from_overseer: mpsc::Receiver<OverseerUpdate>,
    execution_result: mpsc::Receiver<Res<Int>>,
    to_overseer: mpsc::Sender<Result<u64, ()>>,
    overseer_pause: single_value_channel::Updater<Duration>,
    external_function_call: mpsc::Receiver<ExternalCall>,
    external_function_answer: mpsc::Sender<Result<Int, String>>,
}

impl Controller {
    pub fn new(pause: Duration) -> Controller {
        Controller {
            pause_time: pause,
            current_phase: None,
            fun_call_history: Vec::new(),
            result: None,
            current_external_call: None,
            external_function_ids: Vec::new(),
            run_stats: RunStats::default(),

            cancelled: false,
            from_overseer: mpsc::channel().1,
            execution_result: mpsc::channel().1,
            to_overseer: mpsc::channel().0,
            overseer_pause: single_value_channel::channel_starting_with(Duration::from_secs(0)).1,
            external_function_answer: mpsc::channel().0,
            external_function_call: mpsc::channel().1,
        }
    }

    pub fn new_no_pause() -> Controller {
        Controller::new(Duration::from_secs(0))
    }

    pub fn new_max_pause() -> Controller {
        // just a large amount of seconds, for some reason u64::MAX caused issues on windows
        Controller::new(Duration::from_secs(u64::from(u32::MAX)))
    }

    /// Modifies the pause time, the duration the interpreter will block for
    /// before non-trivial AST interpretation waiting for a #unpause() or #cancel() call
    /// after the duration execution will continue automatically
    pub fn set_pause_duration(&mut self, pause_time: Duration) {
        self.pause_time = pause_time;
        // ignore errors to allow setting pause_time before execution
        let _ = self.overseer_pause.update(self.pause_time);
    }

    pub fn pause_duration(&self) -> Duration {
        self.pause_time
    }

    /// Unblocks current waiting phase's execution, if it is blocked.
    /// Requires a recent (in terms of set pause_time) call to #refresh() to be valid
    pub fn unpause(&mut self) {
        if let Some(Phase {
            id,
            unpaused: false,
            ..
        }) = self.current_phase
        {
            // ignore errors as send can happen after execution finishes
            let _ = self.to_overseer.send(Ok(id));
            self.current_phase.as_mut().unwrap().unpaused = true;
        }
    }

    /// Requests any current executing interpreter be cancelled
    pub fn cancel(&mut self) {
        if !self.cancelled {
            self.cancelled = true;
            self.fun_call_history.clear();
            let _ = self.to_overseer.send(Err(()));
        }
    }

    /// Returns current execution phase, requires a recent (in terms of set pause_time)
    /// call to #refresh() to be valid
    pub fn current_phase(&self) -> Option<Phase> {
        if self.cancelled {
            None
        } else {
            self.current_phase.clone()
        }
    }

    pub fn run_stats(&self) -> &RunStats {
        &self.run_stats
    }

    /// Returns result of execution
    pub fn result(&self) -> Option<&Res<Int>> {
        self.result.as_ref()
    }

    /// Returns current execution is paused, requires a recent (in terms of set pause_time)
    /// call to #refresh() to be valid
    pub fn paused(&self) -> bool {
        if self.cancelled {
            return false;
        }

        if let Some(Phase { time, unpaused, .. }) = self.current_phase {
            !unpaused && time.elapsed() < self.pause_time
        } else {
            false
        }
    }

    /// Communicate with the current execution
    /// populates #result & #current_phase if available
    /// Returns if a change has taken place
    pub fn refresh(&mut self) -> bool {
        if self.result.is_some() {
            return false;
        }

        if let Ok(result) = self.execution_result.try_recv() {
            self.result = Some(result);
            self.current_phase = None;
            return true;
        }

        let mut change = self.refresh_overseer_updates();

        if let Ok(call) = self.external_function_call.try_recv() {
            // refresh overseer updates again in case the external call
            // occurred just after the last `refresh_overseer_updates` call
            // call before setting `current_external_call` as it has an assert
            self.refresh_overseer_updates();

            self.current_external_call = Some(call);
            change = true;
        }

        change
    }

    /// Returns if a change occurred
    #[inline]
    fn refresh_overseer_updates(&mut self) -> bool {
        let mut change = false;
        while let Ok(update) = self.from_overseer.try_recv() {
            debug_assert!(
                self.current_external_call.is_none(),
                "Update received during external call: {:?}\nupdate: {:?}",
                self.current_external_call,
                update,
            );

            match update {
                OverseerUpdate::Phase(mut phase) => {
                    phase.called_from = self.current_call_info();
                    if phase.kind == PhaseKind::FunctionCall {
                        self.fun_call_history.push(phase.src)
                    }
                    if !self.cancelled {
                        self.run_stats.consider(&phase);
                    }
                    self.current_phase = Some(phase);
                }
                OverseerUpdate::FinishedFunCall(_) => {
                    self.fun_call_history.pop();
                }
            };
            change = true;
        }
        change
    }

    fn current_call_info(&self) -> Vec<SourceRef> {
        if self.cancelled {
            vec![]
        } else {
            self.fun_call_history.iter().rev().cloned().collect()
        }
    }

    /// Adds an external function signature
    /// must be a valid badder signature to work
    /// ie `some_ext_function(svv)` for a 3 arg function taking a sequence & 2 variable arguments
    pub fn add_external_function(&mut self, id: &str) {
        self.external_function_ids.push(Token::Id(id.into()));
    }

    pub fn clear_external_functions(&mut self) {
        self.external_function_ids.clear();
    }

    pub fn current_external_call(&self) -> Option<ExternalCall> {
        if self.cancelled {
            None
        } else {
            self.current_external_call.clone()
        }
    }

    pub fn answer_external_call(&mut self, result: Result<Int, String>) {
        self.current_external_call = None;
        if let Err(err) = self.external_function_answer.send(result) {
            warn!(
                "Comms failure with badder runtime when answering external call: {}",
                err
            );
        }
    }

    /// Start executing code with a new thread.
    /// Effectively resets this instances state.
    pub fn execute(&mut self, code: Ast) {
        let (to_controller, from_overseer) = mpsc::channel();
        let (to_overseer, from_controller) = mpsc::channel();
        let (final_result, execution_result) = mpsc::channel();
        let (get_pause, set_pause) = single_value_channel::channel_starting_with(self.pause_time);
        let (send_fun_call, recv_fun_call) = mpsc::channel();
        let (send_fun_answer, recv_fun_answer) = mpsc::channel();

        self.to_overseer = to_overseer;
        self.from_overseer = from_overseer;
        self.execution_result = execution_result;
        self.overseer_pause = set_pause;
        self.external_function_call = recv_fun_call;
        self.external_function_answer = send_fun_answer;
        self.result = None;
        self.current_phase = None;
        self.fun_call_history.clear();
        self.current_external_call = None;
        self.cancelled = false;
        self.run_stats = RunStats::default();

        let external_function_ids = self.external_function_ids.clone();

        thread::Builder::new()
            .name("badder-exe".into())
            .stack_size(STACK_SIZE)
            .spawn(move || {
                let overseer = ControllerOverseer {
                    next_id: 0,
                    pause_time: get_pause,
                    to_controller,
                    from_controller,
                    external_function_ids,
                    external_function_call: send_fun_call,
                    external_function_answer: recv_fun_answer,
                    last_stack_copy: None,
                };
                let _ =
                    final_result.send(Interpreter::new(BADDER_STACK_LEN, overseer).evaluate(&code));
            })
            .unwrap();
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ExternalCall {
    pub id: Token,
    pub args: Vec<Int>,
}

impl ExternalCall {
    pub fn id_str(&self) -> &str {
        match self.id {
            Token::Id(ref s) => s,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct RunStats {
    /// source_ref -> evaluation count
    pub eval_counts: IndexMap<SourceRef, usize>,
    last_phase: Option<u64>,
}

impl RunStats {
    fn consider(&mut self, phase: &Phase) {
        if phase.kind != PhaseKind::Assignment {
            if let Some(id) = self.last_phase {
                if id != phase.id {
                    *(self.eval_counts.entry(phase.src).or_insert(0)) += 1;
                }
            } else {
                *(self.eval_counts.entry(phase.src).or_insert(0)) += 1;
            }

            self.last_phase = Some(phase.id);
        }
    }
}
