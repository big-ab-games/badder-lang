mod lexer;
mod parser;

use lexer::Token::*;
use parser::*;

pub type Res<T> = Result<T, String>;
pub type Int = i32;

fn interpret(ast: Ast) -> Res<Int> {
    match ast {
        Ast::Num(Num(x)) => Ok(x),
        Ast::BinOp(token, left, right) => match token {
            Pls => Ok(interpret(*left)? + interpret(*right)?),
            Sub => Ok(interpret(*left)? - interpret(*right)?),
            Mul => Ok(interpret(*left)? * interpret(*right)?),
            Div => Ok(interpret(*left)? / interpret(*right)?),
            _ => Err(format!("Interpreter: Unexpected BinOp token {:?}", token)),
        },
        Ast::LeftUnaryOp(Sub, val) => Ok(-interpret(*val)?),
        _ => Err(format!("Interpreter: Unexpected Ast {:?}", ast)),
    }
}


pub fn eval(code: &str) -> Res<Int> {
    interpret(Parser::new(code)?.parse()?)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn num() {
        assert_eq!(eval("0"), Ok(0));
        assert_eq!(eval("123"), Ok(123));
    }

    #[test]
    fn negative() {
        assert_eq!(eval("-235"), Ok(-235));
    }

    #[test]
    fn double_negative_err() {
        let out = eval("--235");
        assert!(out.is_err(), format!("Unexpected {:?}", out));
        if let Err(reason) = out {
            assert!(reason.contains("-"), format!("Unexpected reason {}", reason));
        }
    }

    #[test]
    fn negative_brace() {
        assert_eq!(eval("-(-45)"), Ok(45));
    }

    #[test]
    fn plus() {
        assert_eq!(eval("12 + 2"), Ok(14));
        assert_eq!(eval("-12 + 2 + 23"), Ok(13));
    }

    #[test]
    fn subtract() {
        assert_eq!(eval("12 - 2"), Ok(10));
        assert_eq!(eval("-12 - 2 - 4"), Ok(-18));
    }

    #[test]
    fn plus_sub() {
        assert_eq!(eval("12 - 2 + 3"), Ok(13));
        assert_eq!(eval("-12 + 2 - 4"), Ok(-14));
    }

    #[test]
    fn multiply() {
        assert_eq!(eval("3 * 4"), Ok(12));
        assert_eq!(eval("4 * -5 * 3"), Ok(-60));
    }

    #[test]
    fn divide() {
        assert_eq!(eval("5 / 2"), Ok(2)); // integer divide
        assert_eq!(eval("-12 / 2 / 2"), Ok(-3));
    }

    #[test]
    fn precidence() {
        assert_eq!(eval("3 + 6 / 2 * 7 + 1"), Ok(25));
    }

    #[test]
    fn negative_mixed_in() {
        assert_eq!(eval("3 + 6 / 2 * -7 + 1"), Ok(-17));
    }

    #[test]
    fn outer_brackets() {
        assert_eq!(eval("(7 + 3 * 4)"), Ok(19));
    }

    #[test]
    fn brackets() {
        assert_eq!(eval("7 + 3 * (10 / (12 / (3 + 1) - 1))"), Ok(22));
    }

    #[test]
    fn redundant_brackets() {
        assert_eq!(eval("7 + (((3 + 2)))"), Ok(12));
    }
}
