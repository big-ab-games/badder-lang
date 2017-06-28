extern crate badder_lang;
extern crate pretty_env_logger;

use badder_lang::*;
use badder_lang::controller::*;
use std::u64;
use std::time::*;

const SRC: &str = "
var loops

# just count up in a loop
while loops < 3
    loops += 1

var plus1 = loops + 1";

macro_rules! await_pause {
    ($controller:ident) => {{
        let before = Instant::now();
        let max_wait = Duration::from_secs(2);
        while !$controller.paused() && before.elapsed() < max_wait {
            $controller.refresh();
        }

        let phase = $controller.current_phase();
        assert!(phase.is_some(), "waited 2 seconds without pause");
        phase.unwrap()
    }}
}

#[test]
fn controller_test() {
    let _ = pretty_env_logger::init();

    let ast = Parser::parse_str(SRC).expect("parse");

    let mut con = controller();
    con.set_unpause_after(Duration::from_secs(u64::MAX));

    con.execute(ast); // `var loops`
    assert_eq!(await_pause!(con).src, SourceRef((2,1), (2,10)));

    con.unpause(); // `while loops < 3`
    assert_eq!(await_pause!(con).src, SourceRef((5,1), (5,16)));

    con.unpause(); // `loops < 3`
    assert_eq!(await_pause!(con).src, SourceRef((5,7), (5,16)));

    con.unpause(); // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));
    con.unpause(); // `loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));

    con.unpause(); // `loops < 3`
    assert_eq!(await_pause!(con).src, SourceRef((5,7), (5,16)));

    con.unpause(); // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));
    con.unpause(); // `loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));

    con.unpause(); // `loops < 3`
    assert_eq!(await_pause!(con).src, SourceRef((5,7), (5,16)));

    con.unpause(); // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));
    con.unpause(); // `loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((6,5), (6,15)));

    con.unpause(); // `loops < 3`
    assert_eq!(await_pause!(con).src, SourceRef((5,7), (5,16)));

    con.unpause(); // `var plus1 = loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((8,1), (8,22)));

    con.unpause(); // `loops + 1`
    assert_eq!(await_pause!(con).src, SourceRef((8,13), (8,22)));
}
