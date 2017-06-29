use super::*;
use std::time::*;
use std::sync::mpsc;
use std::thread;
use std::sync::mpsc::{TryRecvError, RecvTimeoutError};
use single_value_channel;

#[derive(Clone, Debug)]
pub struct Phase {
    pub id: u64,
    pub time: Instant,
    pub src: SourceRef,
    pub stack: Arc<Vec<HashMap<Token, FrameData>>>,
}

struct ControllerOverseer {
    next_id: u64,
    pause_time: single_value_channel::Receiver<Duration>,
    to_controller: mpsc::Sender<Phase>,
    from_controller: mpsc::Receiver<Result<u64, ()>>,
    last_stack_copy: Option<Arc<Vec<HashMap<Token, FrameData>>>>,
}

fn interested_in(ast: &Ast) -> bool {
    use Ast::*;
    match *ast {
        LinePair(..) |
        Line(..) |
        Empty(..) |
        Num(..) |
        ReferSeq(..) |
        ReferSeqIndex(..) |
        Refer(..) => false,
        _ => true
    }
}

impl ControllerOverseer {
    fn replace_last_stack(&mut self, stack: &[HashMap<Token, FrameData>])
        -> Arc<Vec<HashMap<Token, FrameData>>>
    {
        let last: Arc<Vec<_>> = Arc::new(stack.into());
        self.last_stack_copy = Some(last.clone());
        last
    }
}

impl Overseer for ControllerOverseer {
    fn oversee(&mut self,
               stack: &[HashMap<Token, FrameData>], // Cow?
               ast: &Ast,
               _current_scope: usize,
               _stack_key: StackKey) -> Result<(), ()> {
        if !interested_in(ast) {
            return Ok(());
        }

        let id = self.next_id;
        let send_time = Instant::now();
        self.next_id += 1;

        let stack = { // kerfuffle to avoid cloning the stack when it hasn't changed
            if let Some(last) = self.last_stack_copy.take() {
                if last.as_slice() == stack {
                    self.last_stack_copy = Some(last.clone());
                    last
                }
                else { self.replace_last_stack(stack) }
            }
            else { self.replace_last_stack(stack) }
        };

        trace!("ControllerOverseer sending: {:?} {:?}", ast.src(), ast);
        self.to_controller.send(Phase {
            id,
            src: ast.src(),
            time: send_time,
            stack: stack,
        }).expect("send");

        let mut recv = self.from_controller.try_recv();
        while recv != Err(TryRecvError::Empty) {
            match recv {
                Ok(Ok(i)) => if i == id {
                    return Ok(());
                },
                Ok(Err(_)) | Err(TryRecvError::Disconnected) => {
                    debug!("ControllerOverseer cancelling: {:?} {:?}", ast.src(), ast);
                    return Err(())
                },
                _ => ()
            }
            recv = self.from_controller.try_recv();
        }

        let pause_time = *self.pause_time.latest();
        if send_time.elapsed() >= pause_time {
            Ok(())
        }
        else {
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
                        return Err(())
                    },
                    _ => (),
                };

                elapsed = send_time.elapsed();
            }
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Controller {
    pause_time: Duration,
    current_phase: Option<Phase>,
    result: Option<Res<Int>>,
    from_overseer: mpsc::Receiver<Phase>,
    from_interpreter: mpsc::Receiver<Res<Int>>,
    to_overseer: mpsc::Sender<Result<u64, ()>>,
    set_overseer: single_value_channel::Updater<Duration>,
}

impl Controller {
    pub fn new(pause: Duration) -> Controller {
        Controller {
            pause_time: pause,
            current_phase: None,
            result: None,
            from_overseer: mpsc::channel().1,
            from_interpreter: mpsc::channel().1,
            to_overseer: mpsc::channel().0,
            set_overseer: single_value_channel::channel_starting_with(Duration::from_secs(0)).1,
        }
    }

    pub fn new_no_pause() -> Controller {
        Controller::new(Duration::from_secs(0))
    }

    pub fn new_max_pause() -> Controller {
        Controller::new(Duration::from_secs(::std::u64::MAX))
    }

    /// Modifies the pause time, the duration the interpreter will block for
    /// before non-trivial AST interpretation waiting for a #unpause() or #cancel() call
    /// after the duration execution will continue automatically
    pub fn set_unpause_after(&mut self, pause_time: Duration) {
        self.pause_time = pause_time;
        // ignore errors to allow setting pause_time before execution
        let _ = self.set_overseer.update(self.pause_time);
    }

    /// Unblocks current waiting phase's execution, if it is blocked.
    /// Requires a recent (in terms of set pause_time) call to #refresh() to be valid
    pub fn unpause(&mut self) {
        if let Some(Phase{ id, .. }) = self.current_phase.take() {
            // ignore errors as send can happen after execution finishes
            let _ = self.to_overseer.send(Ok(id));
        }
    }

    /// Requests any current executing interpreter be cancelled
    pub fn cancel(&mut self) {
        let _ = self.current_phase.take();
        let _ = self.to_overseer.send(Err(()));
    }

    /// Returns current execution phase, requires a recent (in terms of set pause_time)
    /// call to #refresh() to be valid
    pub fn current_phase(&self) -> Option<Phase> {
        self.current_phase.clone()
    }

    /// Returns result of execution
    pub fn result(&self) -> Option<&Res<Int>> {
        self.result.as_ref()
    }

    /// Returns current execution is paused, requires a recent (in terms of set pause_time)
    /// call to #refresh() to be valid
    pub fn paused(&self) -> bool {
        if let Some(Phase { time, .. }) = self.current_phase {
            time.elapsed() < self.pause_time
        }
        else { false }
    }

    /// Communicate with the current execution
    /// populates #result & #current_phase if available
    /// Returns if a change has taken place
    pub fn refresh(&mut self) -> bool {
        if self.result.is_some() {
            return false;
        }

        if let Ok(result) = self.from_interpreter.try_recv() {
            self.result = Some(result);
            return true;
        }

        let mut change = false;
        while let Ok(phase) = self.from_overseer.try_recv() {
            self.current_phase = Some(phase);
            change = true;
        }
        change
    }

    /// Start executing code with a new thread.
    /// Effectively resets this instances state.
    pub fn execute(&mut self, code: Ast) {
        let (to_controller, from_overseer) = mpsc::channel();
        let (to_overseer, from_controller) = mpsc::channel();
        let (final_result, from_interpreter) = mpsc::channel();
        let (get_pause, set_pause) = single_value_channel::channel_starting_with(self.pause_time);

        self.to_overseer = to_overseer;
        self.from_overseer = from_overseer;
        self.from_interpreter = from_interpreter;
        self.set_overseer = set_pause;
        self.result = None;
        self.current_phase = None;

        thread::spawn(move|| {
            let overseer = ControllerOverseer {
                next_id: 0,
                pause_time: get_pause,
                to_controller,
                from_controller,
                last_stack_copy: None,
            };
            let _ = final_result.send(Interpreter::new(overseer).evaluate(&code));
        });
    }
}
