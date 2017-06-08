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
#[macro_use]
mod util {
    use super::*;

    pub fn result(code: &str) -> Int {
        eval(code).unwrap()
    }

    pub fn error(code: &str) -> String {
        let out = eval(code);
        assert!(out.is_err(), format!("Unexpected {:?}", out));
        if let Err(reason) = out {
            return reason;
        }
        unreachable!();
    }

    /// assert_program!("..." => 123)
    /// Assert that a program outputs a given result
    /// assert_program!("..." =>X "Three dots are not a program", "you are not trying")
    /// Assert a program fails & the error message includes given substrings (case insensitive)
    #[macro_export]
    macro_rules! assert_program {
        ($( $code:expr );+ => $out:expr) => {{
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            assert_eq!(util::result(&code), $out);
        }};
        ($code:expr =>X $( $sub:expr ),+ ) => {
            let err = util::error($code);
            let err_lower = err.to_lowercase();
            $(
                let substring_lower = $sub.to_lowercase();
                assert!(err_lower.contains(substring_lower.as_str()),
                    format!("Substring:`{}` not in error: {}", $sub, err));
            )+
        };
    }
}

#[cfg(test)]
mod single_scope {
    use super::*;

    #[test]
    fn blank_program() {
        assert_program!("" => 0);
        assert_program!("\n" => 0);
        assert_program!("\n\n\n\n\n" => 0);
    }

    #[test]
    fn multi_line() {
        assert_program!("";
                        "12";
                        "";
                        "123" => 123);
    }

    #[test]
    fn variable_assignment() {
        assert_program!("var one = 1";
                        "var two = 1 + 1";
                        "";
                        "one + two" => 3);
    }

    #[test]
    fn variable_mutation() {
        assert_program!("var one = 1";
                        "one = 3";
                        "one" => 3);
        assert_program!("var one = 1";
                        "one = one + 50";
                        "one" => 51);
    }

    #[test]
    fn variable_reassignment() {
        assert_program!("var b = 1";
                        "var b = 2";
                        "b" => 2);
        assert_program!("var a = 1";
                        "var a = a * 3";
                        "a" => 3);
    }

    #[test]
    fn reassignment_no_assign_err() {
        assert_program!("b = 1" =>X "`b`", "not found");
    }

    #[test]
    fn comments() {
        assert_program!("var b = 1 # assign b to 1";
                        "var b = 2 # same effect as `b = 2`";
                        "b" => 2);
        assert_program!("## start with some notes";
                        "123 # just return a number";
                        "";
                        "";
                        "#### some closing comment..." => 123);
    }

    #[test]
    fn empty_declaration() {
        assert_program!("var b";
                        "b" => 0);
    }

    #[test]
    fn operator_assignment() {
        assert_program!("var v = 12";
                        "v += 3"; // 15
                        "v -= 5"; // 10
                        "v *= 2"; // 20
                        "v /= 4";
                        "v" => 5)
    }
}

#[cfg(test)]
mod numerics {
    use super::*;

    #[test]
    fn num() {
        assert_program!("0" => 0);
        assert_program!("123" => 123);
    }

    #[test]
    fn negative() {
        assert_program!("-235" => -235);
    }

    #[test]
    fn double_negative_err() {
        assert_program!("--235" =>X "-");
    }

    #[test]
    fn negative_brace() {
        assert_program!("-(-45)" => 45);
    }

    #[test]
    fn plus() {
        assert_program!("12 + 2" => 14);
        assert_program!("-12 + 2 + 23" => 13);
    }

    #[test]
    fn subtract() {
        assert_program!("12 - 2" => 10);
        assert_program!("-12 - 2 - 4" => -18);
    }

    #[test]
    fn plus_sub() {
        assert_program!("12 - 2 + 3" => 13);
        assert_program!("-12 + 2 - 4" => -14);
    }

    #[test]
    fn multiply() {
        assert_program!("3 * 4" => 12);
        assert_program!("4 * -5 * 3" => -60);
    }

    #[test]
    fn divide() {
        assert_program!("5 / 2" => 2); // integer divide
        assert_program!("-12 / 2 / 2" => -3);
    }

    #[test]
    fn precidence() {
        assert_program!("3 + 6 / 2 * 7 + 1" => 25);
    }

    #[test]
    fn negative_mixed_in() {
        assert_program!("3 + 6 / 2 * -7 + 1" => -17);
    }

    #[test]
    fn outer_brackets() {
        assert_program!("(7 + 3 * 4)" => 19);
    }

    #[test]
    fn brackets() {
        assert_program!("7 + 3 * (10 / (12 / (3 + 1) - 1))" => 22);
    }

    #[test]
    fn redundant_brackets() {
        assert_program!("7 + (((3 + 2)))" => 12);
    }
}
