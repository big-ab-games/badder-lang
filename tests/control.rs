#![allow(unknown_lints, clippy::cyclomatic_complexity)]

use assert_matches::assert_matches;
use badder_lang::{controller::*, *};
use std::time::*;

const NO_FLAG: IntFlag = 0;

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
    }};
}

const SRC: &str = "
var loops

# just count up in a loop
while loops < 3
    loops += 1

var plus1 = loops + 1";

#[test]
fn controller_step_test() {
    let _ = env_logger::try_init();

    let ast = Parser::parse_str(SRC).expect("parse");

    let mut con = Controller::new_max_pause();
    con.execute(ast);
    // `var loops`
    assert_eq!(await_next_pause!(con).src, SourceRef((2, 1), (2, 10)));
    // `while loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5, 1), (5, 16)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5, 7), (5, 16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5, 7), (5, 16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5, 7), (5, 16)));
    // `loops += 1` -> `loops = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((6, 5), (6, 15)));
    // `loops < 3`
    assert_eq!(await_next_pause!(con).src, SourceRef((5, 7), (5, 16)));
    // `var plus1 = loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((8, 1), (8, 22)));
    // `loops + 1`
    assert_eq!(await_next_pause!(con).src, SourceRef((8, 13), (8, 22)));
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
            if let Some(&FrameData::Value(val, ..)) = stack[index].get(&id) {
                assert_eq!(val, $int, "unexpected value for `{:?}`", id);
            }
            else {
                assert!(false, "No value for `{:?}` on stack", id);
            }
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
    let _ = env_logger::try_init();

    let ast = Parser::parse_str(STACK_SRC).expect("parse");

    let mut con = Controller::new_max_pause();
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

const EXTERNAL_FUN_SRC: &str = "\
# expect an external function `external_sum(nn)`
var a = 12
fun plus_one(n)
    n + 1

var a123 = external_sum(123, a)

a.plus_one().external_sum(a123)
";

#[test]
fn external_functions_num_args() {
    let _ = env_logger::try_init();
    let start = Instant::now();

    let ast = Parser::parse_str(EXTERNAL_FUN_SRC).expect("parse");

    let mut con = Controller::new_no_pause();
    con.add_external_function("external_sum(vv)");
    con.execute(ast);

    loop {
        // call 1
        if let Some(call) = con.current_external_call() {
            assert_eq!(call.id, Token::Id("external_sum(vv)".into()));
            assert_eq!(call.args, vec![(123, NO_FLAG), (12, NO_FLAG)]);
            con.answer_external_call(Ok((135, NO_FLAG)));
            break;
        }
        assert!(start.elapsed() < Duration::from_secs(2), "Waited 2 seconds for expectation");
        con.refresh();
        assert_matches!(con.result(), None);
    }

    loop {
        // call 2
        if let Some(call) = con.current_external_call() {
            assert_eq!(call.id, Token::Id("external_sum(vv)".into()));
            assert_eq!(call.args, vec![(13, NO_FLAG), (135, NO_FLAG)]);
            con.answer_external_call(Ok((-123, NO_FLAG))); // can be anything, obvs
            break;
        }
        assert!(start.elapsed() < Duration::from_secs(2), "Waited 2 seconds for expectation");
        con.refresh();
        assert_matches!(con.result(), None);
    }

    while con.result().is_none() && start.elapsed() < Duration::from_secs(2) {
        con.refresh();
    }
    assert_matches!(con.result(), Some(&Ok(-123)));
}

const CALLED_FROM_SRC: &str = "\
var x
fun do_a()
    if x < 2  # -> [7, 8], [7, 4, 7, 8]
        do_b()  # -> [7, 8]
fun do_b()
    x += 1  # -> [8], [4, 7, 8]
    do_a()  # -> [8], [4, 7, 8]
do_b()
";

#[test]
fn called_from_info() {
    let _ = env_logger::try_init();
    let ast = Parser::parse_str(CALLED_FROM_SRC).expect("parse");

    macro_rules! assert_called_from_line {
        ($phase:expr => $lines:expr) => {{
            let phase = $phase;
            let simplified_actual: Vec<_> = phase.called_from.iter().map(|src| (src.0).0).collect();
            let lines: Vec<usize> = $lines;
            if lines != simplified_actual {
                println!("Unexpected called_from from phase at {:?}", phase.src);
                assert_eq!(simplified_actual, lines)
            }
        }};
    }

    let mut con = Controller::new_max_pause();
    con.execute(ast);

    // `var x`, `fun do_a()`, `fun do_b()`, `do_b()`
    assert_called_from_line!(await_next_pause!(con) => vec![]);
    assert_called_from_line!(await_next_pause!(con) => vec![]);
    assert_called_from_line!(await_next_pause!(con) => vec![]);
    assert_called_from_line!(await_next_pause!(con) => vec![]);
    // `x = _`, `x + 1`, `do_a()`
    assert_called_from_line!(await_next_pause!(con) => vec![8]);
    assert_called_from_line!(await_next_pause!(con) => vec![8]);
    assert_called_from_line!(await_next_pause!(con) => vec![8]);
    // `if _`, `x < 2`, `do_b()
    assert_called_from_line!(await_next_pause!(con) => vec![7, 8]);
    assert_called_from_line!(await_next_pause!(con) => vec![7, 8]);
    assert_called_from_line!(await_next_pause!(con) => vec![7, 8]);
    // `x =`, `x + 1`, `do_a()`
    assert_called_from_line!(await_next_pause!(con) => vec![4, 7, 8]);
    assert_called_from_line!(await_next_pause!(con) => vec![4, 7, 8]);
    assert_called_from_line!(await_next_pause!(con) => vec![4, 7, 8]);
    // `if _`, `x < 2`
    assert_called_from_line!(await_next_pause!(con) => vec![7, 4, 7, 8]);
    assert_called_from_line!(await_next_pause!(con) => vec![7, 4, 7, 8]);
}
