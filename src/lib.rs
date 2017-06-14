#[macro_use] extern crate log;

mod lexer;
mod parser;

use std::collections::HashMap;
use std::collections::HashSet;
use lexer::Token;
use lexer::Token::*;
use parser::*;
use std::cmp::max;
use std::usize;

pub use parser::Parser;

pub type Res<T> = Result<T, String>;
pub type Int = i32;

const MAX_STACK: usize = 50;

#[derive(Debug, Clone)]
enum FrameData {
    Value(Int),
    Callable(Box<Ast>),
    LoopMarker,
}

#[derive(Debug)]
enum InterpreterUpFlow {
    Error(String),
    LoopBreak,
    LoopContinue,
    FunReturn(Int),
}

use InterpreterUpFlow::*;
use FrameData::*;

#[derive(Debug)]
pub struct Interpreter {
    stack: Vec<HashMap<lexer::Token, FrameData>>,
}

#[inline]
fn bool_to_num(b: bool) -> Int {
    match b {
        true => 1,
        false => 0,
    }
}

fn interprerror<S: Into<String>>(desc: S) -> Result<Int, InterpreterUpFlow> {
    Err(Error(desc.into()))
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

    fn highest_frame_idx(&self, key: &lexer::Token, current_scope: usize, restrict_ref: Option<usize>)
        -> Option<usize>
    {
        let mut idx = current_scope;
        loop {
            let restricted = match restrict_ref {
                Some(i) => idx != current_scope && idx > i,
                None => false,
            };
            if !restricted && self.stack[idx].contains_key(key) {
                return Some(idx);
            }
            if idx == 0 { break }
            idx -= 1;
        }
        None
    }

    fn log_eval(&mut self, ast: &Ast, current_scope: usize, restrict_ref: Option<usize>) {
        match ast {
            &Ast::Line(_,_)|&Ast::LinePair(_,_)|&Ast::Num(_) => (),
            _ => {
                let restrict = match restrict_ref {
                    Some(restriction) => format!(", refer <= {}", restriction),
                    None => "".into(),
                };
                trace!("base: {}{}\nstack: {:?}\neval({:?})",
                    current_scope, restrict, self.stack, ast)
            },
        }
    }

    /// Evaluates the passed in syntax tree
    /// :current_scope the stack frame index currently running in
    /// :restrict_ref optional max frame index referencable
    ///    (not including >= current_scope, which is always ok)
    fn eval(&mut self, ast: Ast, current_scope: usize, restrict_ref: Option<usize>)
        -> Result<Int, InterpreterUpFlow> {
        if log_enabled!(log::LogLevel::Trace) {
            self.log_eval(&ast, current_scope, restrict_ref);
        }

        // syntax noise reducers/shortcuts
        macro_rules! eval {
            ($expr:expr) => { self.eval($expr, current_scope, restrict_ref) };
        }
        macro_rules! highest_frame_idx {
            ($index:expr) => { self.highest_frame_idx($index, current_scope, restrict_ref) };
        }

        match ast {
            Ast::Num(Num(x)) => Ok(x),
            Ast::BinOp(token, left, right) => match token {
                Pls => Ok(eval!(*left)? + eval!(*right)?),
                Sub => Ok(eval!(*left)? - eval!(*right)?),
                Mul => Ok(eval!(*left)? * eval!(*right)?),
                Mod => Ok(eval!(*left)? % eval!(*right)?),
                Div => match eval!(*right)? {
                    0 => interprerror("Interpreter: Cannot divide by zero"),
                    divisor => Ok(eval!(*left)? / divisor)
                },
                And => match eval!(*left)? {
                    0 => Ok(0),
                    _ => eval!(*right)
                },
                Or => match eval!(*left)? {
                    0 => eval!(*right),
                    x => Ok(x)
                },
                Is => Ok(bool_to_num(eval!(*left)? == eval!(*right)?)),
                Gt => Ok(bool_to_num(eval!(*left)? > eval!(*right)?)),
                Lt => Ok(bool_to_num(eval!(*left)? < eval!(*right)?)),
                GtEq => Ok(bool_to_num(eval!(*left)? >= eval!(*right)?)),
                LtEq => Ok(bool_to_num(eval!(*left)? <= eval!(*right)?)),
                _ => interprerror(format!("Interpreter: Unexpected BinOp token `{:?}`", token)),
            },
            Ast::LeftUnaryOp(Sub, val) => Ok(-eval!(*val)?),
            Ast::LeftUnaryOp(Not, val) => Ok(match eval!(*val)? {
                0 => 1,
                _ => 0,
            }),
            Ast::Assign(id, expr) => {
                let v = eval!(*expr)?;
                self.stack[current_scope].insert(id.clone(), Value(v));
                Ok(v)
            },
            Ast::Reassign(id, expr) => {
                // reassign to any parent scope
                if let Some(idx) = highest_frame_idx!(&id) {
                    let v = eval!(*expr)?;
                    *self.stack[idx].get_mut(&id).unwrap() = Value(v);
                    Ok(v)
                }
                else {
                    interprerror(format!("{}, did you mean `var {:?} =`?", self.unknown_id_err(&id), id))
                }
            },
            Ast::Refer(id) => {
                if let Some(idx) = highest_frame_idx!(&id) {
                    match self.stack[idx][&id] {
                        Value(v) => Ok(v),
                        _ => interprerror(format!("Interpreter: Invalid reference to non value\
                            `{:?}`", id)),
                    }
                }
                else {
                    interprerror(self.unknown_id_err(&id))
                }
            },
            Ast::If(expr, block, else_line, _) => Ok(match eval!(*expr)? {
                0 => match else_line {
                    Some(else_line) => eval!(*else_line)?,
                    None => 0,
                },
                _ => {
                    eval!(*block)?;
                    0
                }
            }),
            Ast::While(expr, block) => {
                if eval!(*expr.clone())? == 0 {
                    return Ok(0);
                }
                let loop_token = Id("#loop".into());
                self.stack[current_scope].insert(loop_token.clone(), LoopMarker);
                loop {
                    match eval!(*block.clone()) {
                        Err(err) => match err {
                            LoopBreak => break,
                            LoopContinue => continue,
                            e => return Err(e),
                        },
                        Ok(_) => (),
                    }
                    if eval!(*expr.clone())? == 0 {
                        break;
                    }
                }
                self.stack[current_scope].remove(&loop_token);
                Ok(0)
            },
            Ast::LoopNav(token) => {
                let loop_token = Id("#loop".into());
                if highest_frame_idx!(&loop_token).is_some() {
                    match token {
                        Break => Err(LoopBreak),
                        Continue => Err(LoopContinue),
                        _ => interprerror(format!("Interpreter: Unknown loop nav `{:?}`", token)),
                    }
                }
                else {
                    interprerror(format!("Interpreter: Invalid use of loop nav `{:?}`", token))
                }
            },
            Ast::Fun(id, block) => {
                let top = self.stack.len() - 1;
                self.stack[top].insert(id, Callable(block));
                Ok(0)
            },
            Ast::Call(id) => {
                if let Some(idx) = highest_frame_idx!(&id) {
                    warn!("Callable {:?} found at index {}", id, idx);
                    match self.stack[idx][&id].clone() {
                        Callable(block) => match self.eval(*block, current_scope+1, Some(idx)) {
                            Err(FunReturn(value)) => Ok(value),
                            Ok(_) => Ok(0),
                            x => x,
                        },
                        _ => interprerror(format!("Interpreter: Invalid reference to\
                            non callable `{:?}`", id)),
                    }
                }
                else {
                    interprerror(self.unknown_id_err(&id))
                }
            },
            Ast::Return(expr) => {
                Err(FunReturn(eval!(*expr)?))
            }
            Ast::Line(scope, expr) => {
                let scope = max(current_scope, scope);
                if scope > MAX_STACK {
                    return interprerror("stack overflow");
                }

                while self.stack.get(scope).is_none() {
                    self.stack.push(HashMap::new());
                }
                while self.stack.get(scope + 1).is_some() {
                    self.stack.pop();
                }
                let out = self.eval(*expr, scope, restrict_ref);
                out
            },
            Ast::LinePair(line, next) => {
                eval!(*line)?;
                Ok(eval!(*next)?)
            },
            Ast::Empty => Ok(0),
            _ => interprerror(format!("Interpreter: Unexpected Ast {:?}", ast)),
        }
    }

    pub fn interpret(&mut self, ast: Ast) -> Res<Int> {
        match self.eval(ast, 0, None) {
            Ok(x) => Ok(x),
            Err(Error(desc)) => Err(desc),
            Err(err) => panic!(err),
        }
    }
}

pub fn eval(code: &str) -> Res<Int> {
    Interpreter::new().interpret(Parser::parse_str(code)?)
}


#[cfg(test)]
#[macro_use]
mod util {
    extern crate pretty_env_logger;

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
        let ast = Parser::parse_str(code)?;
        println!("Program: \n{}", ast.debug_string());
        Ok(())
    }

    pub fn result(code: &str) -> Int {
        let _ = pretty_env_logger::init();

        print_program_debug(code).unwrap();
        eval_within(code, Duration::from_secs(1)).unwrap()
    }

    pub fn error(code: &str) -> String {
        let _ = pretty_env_logger::init();

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
mod functions {
    use super::*;

    #[test]
    fn basic_function() {
        assert_program!("fun two()";
                        "    return 2";
                        "two()" => 2);
    }

    #[test]
    fn function_scope() {
        assert_program!("var n = 0";
                        "fun npp()";
                        "    n += 1";
                        "    return n";
                        "var n1 = npp()";
                        "var n2 = npp()";
                        "n1 + n2" => 3);
    }

    #[test]
    fn function_ref_scope() {
        assert_program!("var out";
                        "var c = -11";
                        "if 1";
                        "    var a = 12"; // Intended
                        "    var b = -5";
                        "    var c = 123";
                        "    if 2";
                        "        fun a_and_b_p1()";
                        "            var c = 1"; // Intended
                        "            return a + b + c";
                        "        var b = 6"; // Intended
                        "        var c = -123123";
                        "        if 3";
                        "            var a = 1";
                        "            var c = 999";
                        // call in scope 3 should have access to scopes >3 and <=2 (def scope)
                        "            out = a_and_b_p1()"; // should refer to a(scope1), b(scope2)
                        "out" => 19);
    }

    #[test]
    fn stack_overflow() {
        assert_program!("fun recurse()";
                        "    recurse()";
                        "recurse()" =>X "stack overflow");
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
