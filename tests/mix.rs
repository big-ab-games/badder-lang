extern crate badder_interpreter;

use badder_interpreter::*;

#[test]
fn mix() {
    let source = include_str!("fortytwo.badder");
    let ast = Parser::new(source).expect("Parser::new").parse().expect("parse");
    let result = Interpreter::new().interpret(ast).expect("interpret");
    assert_eq!(result, 42);
}
