#[macro_use] extern crate assert_matches;
extern crate badder_lang;
extern crate pretty_env_logger;

use badder_lang::*;
use badder_lang::controller::*;
use std::u64;
use std::time::*;

macro_rules! await_next_pause {
    ($controller:ident) => {{
        $controller.refresh();
        if $controller.paused() {
            $controller.unpause();
        }

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

const SRC: &str = "
var loops

# just count up in a loop
while loops < 3
    loops += 1

var plus1 = loops + 1";

#[test]
fn controller_step_test() {
    let _ = pretty_env_logger::init();

    let ast = Parser::parse_str(SRC).expect("parse");

    let mut con = controller();
    con.set_unpause_after(Duration::from_secs(u64::MAX));

    con.execute(ast);
    // `var loops`
    assert_eq!(await_next_pause!(con).src, SourceRef((2,1), (2,10)));
    // `while loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5,1), (5,16)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5,7), (5,16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5,7), (5,16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5,7), (5,16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
     // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6,5), (6,15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5,7), (5,16)));
    // `var plus1 = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((8,1), (8,22)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((8,13), (8,22)));
}

macro_rules! assert_stack {
    ($stack:expr; $( $key:ident = ($index:expr, $int:expr) ),*) => {{
        let stack = $stack;
        $(
            let id = Token::Id(stringify!($key).into());
            let index = $index;
            if stack.len() <= index || !stack[index].contains_key(&id) {
                println!("Actual stack {:?}", stack);
                assert!(stack.len() > index, "Stack smaller than expected");
            }
            assert_matches!(stack[index].get(&id), Some(&FrameData::Value($int)));
        )*
    }};
}

const STACK_SRC: &str = "
var a = 123  # 1
if a is 123  # 2,3
    var b = 500  # 4
    if b is 500  # 5,6
        var c = 17  # 7
        a = 30  # 8
a + 1  # 9";

#[test]
fn phase_stack() {
    let _ = pretty_env_logger::init();

    let ast = Parser::parse_str(STACK_SRC).expect("parse");

    let mut con = controller();
    con.set_unpause_after(Duration::from_secs(u64::MAX));

    con.execute(ast);
    // 1: [{}]
    await_next_pause!(con);
    // 2,3,4: [{a: 123}]
    assert_stack!(await_next_pause!(con).stack; a = (0, 123));
    assert_stack!(await_next_pause!(con).stack; a = (0, 123));
    assert_stack!(await_next_pause!(con).stack; a = (0, 123));
    // 5,6,7: [_, {b: 500}]
    assert_stack!(await_next_pause!(con).stack; a = (0, 123), b = (1, 500));
    assert_stack!(await_next_pause!(con).stack; a = (0, 123), b = (1, 500));
    assert_stack!(await_next_pause!(con).stack; a = (0, 123), b = (1, 500));
    // 8: [_, _, {c: 17}]
    assert_stack!(await_next_pause!(con).stack; a = (0, 123), b = (1, 500), c = (2, 17));
    // 9: [{a: 30}]
    assert_stack!(await_next_pause!(con).stack; a = (0, 30));

    con.unpause();
    let before_result = Instant::now();
    while con.result().is_none() && before_result.elapsed() < Duration::from_secs(2) {
        con.refresh();
    }
    assert_matches!(con.result(), Some(&Ok(31)));
}
