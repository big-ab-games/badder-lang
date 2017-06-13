mod lexer;
mod parser;

use std::collections::HashMap;
use std::collections::HashSet;
use lexer::Token;
use lexer::Token::*;
use parser::*;

pub use parser::Parser;

pub type Res<T> = Result<T, String>;
pub type Int = i32;

#[derive(Debug)]
pub struct Interpreter {
    stack: Vec<HashMap<lexer::Token, Int>>,
}

#[inline]
fn bool_to_num(b: bool) -> Int {
    match b {
        true => 1,
        false => 0,
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            stack: vec!(HashMap::new())
        }
    }

    fn unknown_id_err(&mut self, id: &Token) -> String {
        let keys: HashSet<&lexer::Token> = self.stack.iter()
            .flat_map(|m| m.keys())
            .collect();
        if keys.is_empty() {
            format!("Interpreter: id `{:?}` not found in scope", id)
        }
        else {
            assert!(!keys.contains(id), "!keys.contains(id): `{:?}` is available", id);
            format!("Interpreter: id `{:?}` not found in scope, {:?} available",
                id, keys)
        }
    }

    fn highest_frame_idx_with(&self, key: &lexer::Token) -> Option<usize> {
        let mut idx = self.stack.len() as isize - 1 ;
        while idx != -1 {
            if self.stack[idx as usize].contains_key(key) {
                return Some(idx as usize);
            }
            idx -= 1;
        }
        None
    }

    pub fn interpret(&mut self, ast: Ast) -> Res<Int> {
        match ast {
            Ast::Num(Num(x)) => Ok(x),
            Ast::BinOp(token, left, right) => match token {
                Pls => Ok(self.interpret(*left)? + self.interpret(*right)?),
                Sub => Ok(self.interpret(*left)? - self.interpret(*right)?),
                Mul => Ok(self.interpret(*left)? * self.interpret(*right)?),
                Mod => Ok(self.interpret(*left)? % self.interpret(*right)?),
                Div => match self.interpret(*right)? {
                    0 => Err("Interpreter: Cannot divide by zero".into()),
                    divisor => Ok(self.interpret(*left)? / divisor)
                },
                And => match self.interpret(*left)? {
                    0 => Ok(0),
                    _ => self.interpret(*right)
                },
                Or => match self.interpret(*left)? {
                    0 => self.interpret(*right),
                    x => Ok(x)
                },
                Is => Ok(bool_to_num(self.interpret(*left)? == self.interpret(*right)?)),
                Gt => Ok(bool_to_num(self.interpret(*left)? > self.interpret(*right)?)),
                Lt => Ok(bool_to_num(self.interpret(*left)? < self.interpret(*right)?)),
                GtEq => Ok(bool_to_num(self.interpret(*left)? >= self.interpret(*right)?)),
                LtEq => Ok(bool_to_num(self.interpret(*left)? <= self.interpret(*right)?)),
                _ => Err(format!("Interpreter: Unexpected BinOp token `{:?}`", token)),
            },
            Ast::LeftUnaryOp(Sub, val) => Ok(-self.interpret(*val)?),
            Ast::LeftUnaryOp(Not, val) => Ok(match self.interpret(*val)? {
                0 => 1,
                _ => 0,
            }),
            Ast::Assign(id, expr) => {
                let v = self.interpret(*expr)?;
                let last_idx = self.stack.len() - 1;
                self.stack[last_idx].insert(id.clone(), v);
                Ok(v)
            },
            Ast::Reassign(id, expr) => {
                // reassign to any parent scope
                if let Some(idx) = self.highest_frame_idx_with(&id) {
                    let v = self.interpret(*expr)?;
                    *self.stack[idx].get_mut(&id).unwrap() = v;
                    Ok(v)
                }
                else {
                    Err(format!("{}, did you mean `var {:?} =`?", self.unknown_id_err(&id), id))
                }
            },
            Ast::Refer(id) => {
                if let Some(idx) = self.highest_frame_idx_with(&id) {
                    Ok(self.stack[idx].get(&id).cloned()
                        .expect("Error highest_frame_idx_with result"))
                }
                else {
                    Err(self.unknown_id_err(&id))
                }
            },
            Ast::If(expr, block, else_line, _) => Ok(match self.interpret(*expr)? {
                0 => match else_line {
                    Some(else_line) => self.interpret(*else_line)?,
                    None => 0,
                },
                _ => {
                    self.interpret(*block)?;
                    0
                }
            }),
            Ast::While(expr, block) => {
                if self.interpret(*expr.clone())? == 0 {
                    return Ok(0);
                }
                let last_idx = self.stack.len() - 1;
                let loop_token = Id("#loop".into());
                self.stack[last_idx].insert(loop_token.clone(), 1);
                while let Some(&1) = self.stack[last_idx].get(&loop_token) {
                    match self.interpret(*block.clone()) {
                        Err(err) => match err.as_str() {
                            "#loop.break" => break,
                            "#loop.continue" => continue,
                            err => return Err(err.into()),
                        },
                        Ok(_) => (),
                    }
                    if self.interpret(*expr.clone())? == 0 {
                        break;
                    }
                }
                self.stack[last_idx].remove(&loop_token);
                Ok(0)
            },
            Ast::LoopNav(token) => match token {
                Break => {
                    let loop_token = Id("#loop".into());
                    if let Some(idx) = self.highest_frame_idx_with(&loop_token) {
                        *self.stack[idx].get_mut(&loop_token).unwrap() = 0;
                        Err("#loop.break".into())
                    }
                    else {
                        Err(format!("Interpreter: Invalid use of loop nav `{:?}`", token))
                    }
                },
                Continue => {
                    let loop_token = Id("#loop".into());
                    if self.highest_frame_idx_with(&loop_token).is_some() {
                        Err("#loop.continue".into())
                    }
                    else {
                        Err(format!("Interpreter: Invalid use of loop nav `{:?}`", token))
                    }
                },
                _ => Err(format!("Interpreter: Unknown loop nav `{:?}`", token)),
            },
            Ast::Line(scope, expr) => {
                while scope + 1 > self.stack.len() {
                    self.stack.push(HashMap::new());
                }
                while scope + 1 < self.stack.len() {
                    self.stack.pop();
                }
                self.interpret(*expr)
            },
            Ast::LinePair(line, next) => {
                self.interpret(*line)?;
                // println!("after line {:?}", self);
                Ok(self.interpret(*next)?)
            },
            Ast::Empty => Ok(0),
            _ => Err(format!("Interpreter: Unexpected Ast {:?}", ast)),
        }
    }
}

pub fn eval(code: &str) -> Res<Int> {
    Interpreter::new().interpret(Parser::new(code)?.parse()?)
}


#[cfg(test)]
#[macro_use]
mod util {
    use super::*;
    use std::sync::mpsc;
    use std::thread;
    use std::time::{Duration, Instant};

    fn eval_within(code: &str, timeout: Duration) -> Res<Int> {
        let (sender, receiver) = mpsc::channel();
        let code: String = code.into();
        thread::spawn(move|| {
            sender.send(eval(&code)).unwrap();
        });

        let now = Instant::now();
        while now.elapsed() < timeout {
            match receiver.try_recv() {
                Ok(res) => return res,
                _ => thread::sleep(Duration::from_millis(5)),
            }
        }
        Err(format!("Program did not return within {:?}", timeout))
    }

    fn print_program_debug(code: &str) -> Res<()> {
        let ast = Parser::new(code)?.parse()?;
        println!("Program: \n{}", ast.debug_string());
        Ok(())
    }

    pub fn result(code: &str) -> Int {
        print_program_debug(code).unwrap();
        eval_within(code, Duration::from_secs(1)).unwrap()
    }

    pub fn error(code: &str) -> String {
        let out = eval_within(code, Duration::from_secs(1));
        if out.is_ok() {
            print_program_debug(code).unwrap();
        }
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
        ($( $code:expr );+ =>X $( $sub:expr ),+ ) => {
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            let err = util::error(&code);
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
mod loops {
    use super::*;

    #[test]
    fn loop_and_break() {
        assert_program!("var x = 1";
                        "loop";
                        "    x *= 2";
                        "    if x > 10";
                        "        break";
                        "x" => 16);
    }

    #[test]
    fn loop_continue_break() {
        assert_program!("var x = 1";
                        "loop";
                        "    x -= 1";
                        "    if x <= -5";
                        "        x *= -3";
                        "        continue";
                        "    if x > 10";
                        "        break";
                        "x" => 14);
    }

    #[test]
    fn while_loop() {
        assert_program!("var x = 1";
                        "while x < 50";
                        "    x *= 3";
                        "x" => 81);
    }

    #[test]
    fn while_continue_break() {
        assert_program!("var x";
                        "while x < 50";
                        "    x += 30";
                        "    if x >= 50";
                        "        break";
                        "    if x % 2 is 1";
                        "        continue";
                        "    x -= 31";
                        "x" => 59);
    }
}

#[cfg(test)]
mod if_scope {
    use super::*;

    #[test]
    fn if_flow() {
        assert_program!("var x = 200";
                        "var y = 100";
                        "if x is 200";
                        "    x = 123";
                        "if y is 200";
                        "    y = 123";
                        "x + y" => 223);
        assert_program!("var x = 0";
                        "var y = 0";
                        "if x is y";
                        "    x = 20";
                        "if x is y";
                        "    y = 30";
                        "x + y" => 20);
    }

    #[test]
    fn if_scope() {
        assert_program!("var x = 200";
                        "var y = 111";
                        "if x is 200";
                        "    var x = 300"; // shadow x in if scope
                        "    y = 300"; // reassign in parent scope
                        "x + y" => 500);
        assert_program!("if 1";
                        "    var x = 234";
                        "x" =>X "x", "scope");
    }

    #[test]
    fn if_junkspace() {
        assert_program!("";
                        "# about to do an if";
                        "var x  # define a juicy var";
                        "";
                        "if x is 0 # this should always work";
                        "";
                        "  "; // non 4x indent, but fully junk
                        "    ";
                        "    # hmm";
                        "         # why did i write this?"; // non 4x indent, but fully junk
                        "    x += 50";
                        "# blankers helps the program be readable maybe";
                        "";
                        "";
                        "    x *= 2";
                        "    ";
                        "    ";
                        "x";
                        "";
                        "# finished now" => 100);
    }

    #[test]
    fn if_scope_err() {
        assert_program!("var x = 200";
                        "if x is not 0";
                        "   x -= 1"; // dodgy indent
                        "    x += 1";
                        "x" =>X "indent");
    }

    #[test]
    fn multi_scope() {
        assert_program!("var x = 200";
                        "if x is not 0";
                        "    var y = x";
                        "    if y is 200";
                        "        var z = y";
                        "        if z is not 999";
                        "            y /= 2";
                        "            x = y + z";
                        "x" => 300);
    }

    #[test]
    fn if_else() {
        assert_program!("var x";
                        "if x";
                        "    x = 14";
                        "else";
                        "    x = -9000";
                        "x" => -9000);
    }

    #[test]
    fn if_else_chain() {
        assert_program!("var x = 3";
                        "if x is 0";
                        "    x = -1";
                        "else if x is 1";
                        "    x = -2";
                        "else if x is 2";
                        "    x = -3";
                        "else if x is 3";
                        "    x = -4";
                        "else if x is 4";
                        "    x = -5";
                        "else";
                        "    x = -999";
                        "x" => -4);
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
mod booleans {
    use super::*;

    #[test]
    fn is() {
        assert_program!("1 is 2" => 0);
        assert_program!("2 is 1 + 1" => 1);
    }

    #[test]
    fn is_not() {
        assert_program!("3 is not 3" => 0);
        assert_program!("3 is not 4" => 1);
    }

    #[test]
    fn not() {
        assert_program!("not 1" => 0);
        assert_program!("not 0" => 1);
        assert_program!("not 5 is 4" => 1);
        assert_program!("not 5 or 4 and 0" => 0);
    }

    #[test]
    fn multi_compare_err() {
        assert_program!("2 is 2 is 1" =>X "is");
        assert_program!("2 > 2 < 1" =>X "<");
        assert_program!("2 > 2 is 0" =>X "is");
    }

    #[test]
    fn and() {
        assert_program!("1 + 2 and 2 + 3 and 3 + 4" => 7);
        assert_program!("not 1 - 1 and not 2 + 3" => 0);
        assert_program!("1 + 2 and 7 is 2 and 3 + 4" => 0);
    }

    #[test]
    fn or() {
        assert_program!("1 + 2 or 2 + 3 or 3 + 4" => 3);
        assert_program!("0 or 1 and 0 or 0" => 0);
        assert_program!("0 or 0 or 123" => 123);
        assert_program!("0 or 234 or 0" => 234);
    }

    #[test]
    fn greater_than() {
        assert_program!("1 > 1" => 0);
        assert_program!("1 > 2" => 0);
        assert_program!("1 > 0" => 1);
        assert_program!("1 > -2" => 1);
    }

    #[test]
    fn less_than() {
        assert_program!("1 < 1" => 0);
        assert_program!("1 < 2" => 1);
        assert_program!("1 < 0" => 0);
        assert_program!("1 < -2" => 0);
    }

    #[test]
    fn greater_than_or_equal_to() {
        assert_program!("1 >= 1" => 1);
        assert_program!("1 >= 2" => 0);
        assert_program!("1 >= 0" => 1);
        assert_program!("1 >= -2" => 1);
    }

    #[test]
    fn less_than_or_equal_to() {
        assert_program!("1 <= 1" => 1);
        assert_program!("1 <= 2" => 1);
        assert_program!("1 <= 0" => 0);
        assert_program!("1 <= -2" => 0);
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
    fn precedence() {
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

    #[test]
    fn div_by_0_err() {
        assert_program!("1/0" =>X "divide by zero");
    }

    #[test]
    fn modulus() {
        assert_program!("52 % 2" => 0);
        assert_program!("53 % 2" => 1);
        assert_program!("53 % 50" => 3);
        assert_program!("14023 % 50" => 23);
    }
}
