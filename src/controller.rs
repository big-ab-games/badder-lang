use super::*;
use std::time::*;
use std::sync::mpsc;
use std::thread;

#[derive(Clone, Debug)]
pub struct Phase {
    time: Instant,
    pub src: SourceRef,
}

pub struct ControllerOverseer {
    to_controller: mpsc::SyncSender<Phase>,
    from_controller: mpsc::Receiver<Result<(), ()>>,
}

fn should_skip(ast: &Ast) -> bool {
    use Ast::*;
    match *ast {
        LinePair(..) |
        Line(..) |
        Empty(..) |
        Num(..) |
        ReferSeq(..) |
        ReferSeqIndex(..) |
        Refer(..) => true,
        _ => false
    }
}

impl Overseer for ControllerOverseer {
    fn oversee(&self,
               stack: &[HashMap<Token, FrameData>],
               ast: &Ast,
               current_scope: usize,
               stack_key: StackKey) -> Result<(), ()> {
        if should_skip(ast) {
            return Ok(());
        }

        info!("ControllerOverseer sending... {:?}", ast);
        self.to_controller.send(Phase {
            src: ast.src(),
            time: Instant::now(),
        }).expect("send");

        // block until result received
        info!("ControllerOverseer waiting... {:?}", ast);
        self.from_controller.recv().expect("recv")
    }
}

pub struct Controller {
    pause_time: Duration,
    current_phase: Option<Phase>,
    result: Option<Res<Int>>,
    from_overseer: mpsc::Receiver<Phase>,
    from_interpreter: mpsc::Receiver<Res<Int>>,
    to_overseer: mpsc::Sender<Result<(), ()>>,
}

pub fn controller() -> Controller {
    Controller {
        pause_time: Duration::from_secs(0),
        current_phase: None,
        result: None,
        from_overseer: mpsc::channel().1,
        from_interpreter: mpsc::channel().1,
        to_overseer: mpsc::channel().0,
    }
}

impl Controller {
    pub fn set_unpause_after(&mut self, pause_time: Duration) {
        self.pause_time = pause_time;
    }

    pub fn unpause(&mut self) {
        if let Some(_) = self.current_phase.take() {
            self.to_overseer.send(Ok(())).expect("to_overseer.send");
        }
    }

    pub fn current_phase(&self) -> Option<Phase> {
        self.current_phase.clone()
    }

    pub fn paused(&self) -> bool {
        self.current_phase.is_some()
    }

    pub fn refresh(&mut self) {
        if self.result.is_some() {
            return;
        }

        if let Ok(result) = self.from_interpreter.try_recv() {
            self.result = Some(result);
            return;
        }

        if let Ok(phase) = self.from_overseer.try_recv() {
            self.current_phase = Some(phase);
        }

        if let Some(Phase{time, ..}) = self.current_phase {
            if time.elapsed() >= self.pause_time {
                self.unpause();
            }
        }
    }

    pub fn execute(&mut self, code: Ast) {
        let (to_controller, from_overseer) = mpsc::sync_channel(0);
        let (to_overseer, from_controller) = mpsc::channel();
        let (final_result, from_interpreter) = mpsc::channel();

        self.to_overseer = to_overseer;
        self.from_overseer = from_overseer;
        self.from_interpreter = from_interpreter;

        thread::spawn(move|| {
            let overseer = ControllerOverseer {
                to_controller,
                from_controller,
            };
            if let Err(err) = final_result.send(Interpreter::new(overseer).evaluate(&code)) {
                debug!("final_result.send: {:?}", err);
            }
        });
    }
}
