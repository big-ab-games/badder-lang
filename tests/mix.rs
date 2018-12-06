

use badder_lang::*;

#[test]
fn mix() {
    let source = include_str!("fortytwo.badder");
    let ast = Parser::parse_str(source).expect("parse");
    let result = Interpreter::default().evaluate(&ast).expect("interpret");
    assert_eq!(result, 42);
}
