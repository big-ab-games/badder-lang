mod lexer;
mod parser;

use std::collections::HashMap;
use lexer::Token;
use lexer::Token::*;
use parser::*;

pub use parser::Parser;

pub type Res<T> = Result<T, String>;
pub type Int = i32;

#[derive(Default)]
pub struct Interpreter {
    vars: HashMap<lexer::Token, Int>,
}

impl Interpreter {
    fn unknown_id_err(&mut self, id: &Token) -> String {
        if self.vars.is_empty() {
            format!("Interpreter: id `{:?}` not found in scope", id)
        }
        else {
            format!("Interpreter: id `{:?}` not found in scope, {:?} available",
                id, self.vars.keys())
        }
    }

    pub fn interpret(&mut self, ast: Ast) -> Res<Int> {
        match ast {
            Ast::Num(Num(x)) => Ok(x),
            Ast::BinOp(token, left, right) => match token {
                Pls => Ok(self.interpret(*left)? + self.interpret(*right)?),
                Sub => Ok(self.interpret(*left)? - self.interpret(*right)?),
                Mul => Ok(self.interpret(*left)? * self.interpret(*right)?),
                Div => Ok(self.interpret(*left)? / self.interpret(*right)?),
                _ => Err(format!("Interpreter: Unexpected BinOp token {:?}", token)),
            },
            Ast::LeftUnaryOp(Sub, val) => Ok(-self.interpret(*val)?),
            Ast::Assign(id, expr) => {
                let v = self.interpret(*expr)?;
                self.vars.insert(id.clone(), v);
                Ok(v)
            },
            Ast::Reassign(id, expr) => {
                // reassign to any parent scope
                if self.vars.contains_key(&id) {
                    let v = self.interpret(*expr)?;
                    *self.vars.get_mut(&id).unwrap() = v;
                    Ok(v)
                }
                else {
                    Err(format!("{}, did you maen `var {:?} =`?",
                        self.unknown_id_err(&id), id))
                }
            },
            Ast::Refer(id) => self.vars.get(&id).cloned()
                .ok_or_else(|| self.unknown_id_err(&id)),
            Ast::Line(line, next) => {
                self.interpret(*line)?;
                Ok(self.interpret(*next)?)
            },
            Ast::Empty => Ok(0),
            _ => Err(format!("Interpreter: Unexpected Ast {:?}", ast)),
        }
    }
}

pub fn eval(code: &str) -> Res<Int> {
    Interpreter::default().interpret(Parser::new(code)?.parse()?)
}


#[cfg(test)]
mod single_scope {
    use super::*;

    #[test]
    fn blank_program() {
        assert_eq!(eval(""), Ok(0));
        assert_eq!(eval("\n"), Ok(0));
        assert_eq!(eval("\n\n\n\n\n"), Ok(0));
    }

    #[test]
    fn multi_line() {
        assert_eq!(eval("\n\
                         12\n\
                         \n\
                         123"),
                   Ok(123));
    }

    #[test]
    fn variable_assignment() {
        assert_eq!(eval("var one = 1\n\
                         var two = 1 + 1\n\
                         \n\
                         one + two"),
                   Ok(3));
    }

    #[test]
    fn variable_mutation() {
        assert_eq!(eval("var one = 1\n\
                         one = 3\n\
                         one"), Ok(3));
        assert_eq!(eval("var one = 1\n\
                         one = one + 50\n\
                         one"),
                   Ok(51));
    }

    #[test]
    fn variable_reassignment() {
        assert_eq!(eval("var b = 1\n\
                         var b = 2\n\
                         b"),
                   Ok(2));
        assert_eq!(eval("var a = 1\n\
                         var a = a * 3\n\
                         a"),
                   Ok(3));
    }

    #[test]
    fn reassignment_no_assign_err() {
        let out = eval("b = 1");
        assert!(out.is_err(), format!("Unexpected {:?}", out));
        if let Err(reason) = out {
            assert!(reason.contains("`b`"), format!("Unexpected reason `{}`", reason));
        }
    }

    #[test]
    fn comments() {
        assert_eq!(eval("var b = 1 # assign b to 1\n\
                         var b = 2 # same effect as `b = 2`\n\
                         b"),
                   Ok(2));
        assert_eq!(eval("## start with some notes\n\
                         123 # just return a number\n\
                         \n\
                         \n\
                         #### some closing comment..."),
                   Ok(123));
    }
}

#[cfg(test)]
mod numerics {
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
