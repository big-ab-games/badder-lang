#[macro_use]
extern crate log;
extern crate string_cache;

mod lexer;
mod parser;

use lexer::Token;
use lexer::Token::*;
use parser::*;
pub use parser::Parser;
use std::cmp::max;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::iter::Iterator;
use std::mem;
use std::sync::Arc;
use std::usize;

pub type Res<T> = Result<T, String>;
pub type Int = i32;

const MAX_STACK: usize = 50;

enum FrameData {
    Value(Int),
    /// Callable(args, block)
    Callable(Vec<Token>, Arc<Ast>),
    LoopMarker,
}

impl fmt::Debug for FrameData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self
        {
            Value(x) => write!(f, "Value({})", x),
            Callable(..) => write!(f, "Callable"),
            LoopMarker => write!(f, "LoopMarker"),
        }
    }
}

#[derive(Debug)]
enum InterpreterUpFlow {
    Error(String),
    LoopBreak,
    LoopContinue,
    FunReturn(Int),
}

use FrameData::*;
use InterpreterUpFlow::*;

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
        Interpreter { stack: vec![HashMap::new()] }
    }

    fn unknown_id_err(&mut self, id: &Token) -> String {
        let keys: HashSet<&lexer::Token> = self.stack.iter().flat_map(|m| m.keys()).collect();
        if keys.is_empty() {
            format!("Interpreter: id `{:?}` not found in scope", id)
        }
        else {
            assert!(!keys.contains(id), "!keys.contains(id): `{:?}` is available", id);
            format!("Interpreter: id `{:?}` not found in scope, {:?} available", id, keys)
        }
    }

    fn highest_frame_idx(&self,
                         key: &lexer::Token,
                         current_scope: usize,
                         restrict_ref: Option<usize>)
                         -> Option<usize> {
        let mut idx = current_scope;
        loop {
            let restricted = match restrict_ref {
                Some(i) => idx != current_scope && idx > i,
                None => false,
            };
            if !restricted && self.stack[idx].contains_key(key) {
                return Some(idx);
            }
            if idx == 0 { break; }
            idx -= 1;
        }
        None
    }

    #[inline]
    fn log_eval(&mut self, ast: &Ast, current_scope: usize, restrict_ref: Option<usize>) {
        match ast {
            &Ast::Line(..) | &Ast::LinePair(..) | &Ast::Num(_) => (),
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
    fn eval(&mut self, ast: &Ast, current_scope: usize, restrict_ref: Option<usize>)
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

        match *ast {
            Ast::Num(Num(x)) => Ok(x),
            Ast::BinOp(ref token, ref left, ref right) => {
                match *token {
                    Pls => Ok(eval!(left)? + eval!(right)?),
                    Sub => Ok(eval!(left)? - eval!(right)?),
                    Mul => Ok(eval!(left)? * eval!(right)?),
                    Mod => Ok(eval!(left)? % eval!(right)?),
                    Div => {
                        match eval!(right)? {
                            0 => interprerror("Interpreter: Cannot divide by zero"),
                            divisor => Ok(eval!(left)? / divisor),
                        }
                    },
                    And => {
                        match eval!(left)? {
                            0 => Ok(0),
                            _ => eval!(right),
                        }
                    },
                    Or => {
                        match eval!(left)? {
                            0 => eval!(right),
                            x => Ok(x),
                        }
                    },
                    Is => Ok(bool_to_num(eval!(left)? == eval!(right)?)),
                    Gt => Ok(bool_to_num(eval!(left)? > eval!(right)?)),
                    Lt => Ok(bool_to_num(eval!(left)? < eval!(right)?)),
                    GtEq => Ok(bool_to_num(eval!(left)? >= eval!(right)?)),
                    LtEq => Ok(bool_to_num(eval!(left)? <= eval!(right)?)),
                    _ => interprerror(format!("Interpreter: Unexpected BinOp token `{:?}`", token)),
                }
            },
            Ast::LeftUnaryOp(Sub, ref val) => Ok(-eval!(val)?),
            Ast::LeftUnaryOp(Not, ref val) => Ok(match eval!(val)? {
                0 => 1,
                _ => 0,
            }),
            Ast::Assign(ref id, ref expr) => {
                let v = eval!(expr)?;
                self.stack[current_scope].insert(id.clone(), Value(v));
                Ok(v)
            },
            Ast::Reassign(ref id, ref expr) => {
                // reassign to any parent scope
                if let Some(idx) = highest_frame_idx!(&id) {
                    let v = eval!(expr)?;
                    *self.stack[idx].get_mut(&id).unwrap() = Value(v);
                    Ok(v)
                }
                else {
                    interprerror(format!("{}, did you mean `var {:?} =`?",
                        self.unknown_id_err(&id), id))
                }
            },
            Ast::Refer(ref id) => {
                if let Some(idx) = highest_frame_idx!(&id) {
                    match self.stack[idx][&id] {
                        Value(v) => Ok(v),
                        _ => interprerror(format!(
                            "Interpreter: Invalid reference to non value `{:?}`",
                            id
                        )),
                    }
                }
                else {
                    interprerror(self.unknown_id_err(&id))
                }
            },
            Ast::If(ref expr, ref block, ref else_line, _) => Ok(match eval!(expr)? {
                0 => {
                    match *else_line {
                        Some(ref else_line) => eval!(else_line)?,
                        None => 0,
                    }
                },
                _ => {
                    eval!(block)?;
                    0
                },
            }),
            Ast::While(ref expr, ref block) => {
                if eval!(expr)? == 0 {
                    return Ok(0);
                }
                let loop_token = Id("#loop".into());
                self.stack[current_scope].insert(loop_token.clone(), LoopMarker);
                loop {
                    match eval!(block) {
                        Err(err) => {
                            match err {
                                LoopBreak => break,
                                LoopContinue => continue,
                                e => return Err(e),
                            }
                        },
                        Ok(_) => (),
                    }
                    if eval!(expr)? == 0 {
                        break;
                    }
                }
                self.stack[current_scope].remove(&loop_token);
                Ok(0)
            },
            Ast::LoopNav(ref token) => {
                let loop_token = Id("#loop".into());
                if highest_frame_idx!(&loop_token).is_some() {
                    match *token {
                        Break => Err(LoopBreak),
                        Continue => Err(LoopContinue),
                        _ => interprerror(format!("Interpreter: Unknown loop nav `{:?}`", token)),
                    }
                }
                else {
                    interprerror(format!("Interpreter: Invalid use of loop nav `{:?}`", token))
                }
            },
            Ast::Fun(ref id, ref args, ref block) => {
                let top = self.stack.len() - 1;
                self.stack[top].insert(id.clone(), Callable(args.clone(), block.clone()));
                Ok(0)
            },
            Ast::Call(ref id, ref args) => {
                if let Some(idx) = highest_frame_idx!(&id) {
                    let (mut arg_ids, callable_block) = {
                        match &self.stack[idx][&id] {
                            &Callable(ref arg_ids, ref block) => (arg_ids.clone(), block.clone()),
                            _ => {
                                return interprerror(format!(
                                    "Interpreter: Invalid reference to non callable `{:?}`",
                                    id
                                ))
                            },
                        }
                    };
                    if args.len() != arg_ids.len() {
                        return interprerror(format!(
                            "Interpreter: `{:?}({})` called with {} argument{}, expects {}",
                            id,
                            arg_ids.iter()
                                .map(|t| format!("{:?}", t))
                                .fold("".into(), |all: String, n| all + &n),
                            args.len(),
                            if arg_ids.len() > 1 { "s" } else { "" },
                            arg_ids.len(),
                        ));
                    }

                    // construct new function call stack frame
                    let mut f_frame = HashMap::new();
                    for i in 0..args.len() {
                        f_frame.insert(mem::replace(&mut arg_ids[i], Eol), Value(eval!(&args[i])?));
                    }
                    self.stack.push(f_frame);

                    let out = match self.eval(&callable_block, current_scope + 1, Some(idx)) {
                        Err(FunReturn(value)) => Ok(value),
                        Ok(x) => Ok(x),
                        x => x,
                    };

                    // clean stack
                    self.stack.pop();
                    out
                }
                else {
                    interprerror(self.unknown_id_err(&id))
                }
            },
            Ast::Return(ref expr) => Err(FunReturn(eval!(expr)?)),
            Ast::Line(scope, ref expr) => {
                let scope = max(current_scope, scope);
                if scope > MAX_STACK {
                    return interprerror("stack overflow");
                }

                while self.stack.len() < scope + 1 {
                    self.stack.push(HashMap::new());
                }
                while self.stack.len() > scope + 1 {
                    self.stack.pop();
                }
                self.eval(expr, scope, restrict_ref)
            },
            Ast::LinePair(ref line, ref next_line) => {
                eval!(line)?;
                let mut next = next_line;
                while let Ast::LinePair(ref l2, ref l3) = **next {
                    eval!(l2)?;
                    next = l3;
                }
                eval!(next)
            },
            Ast::Empty => Ok(0),
            _ => interprerror(format!("Interpreter: Unexpected Ast {:?}", ast)),
        }
    }

    pub fn interpret(&mut self, ast: Ast) -> Res<Int> {
        match self.eval(&ast, 0, None) {
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
        let before_parse = Instant::now();
        debug!("parsing...");
        let code: Ast = Parser::parse_str(code.into())?;
        debug!("parsed in {:?}, interpreting...", before_parse.elapsed());

        thread::spawn(move || {
            let before_interp = Instant::now();
            sender.send(Interpreter::new().interpret(code)).unwrap();
            debug!("interpreted in {:?}", before_interp.elapsed());
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
        debug!("Program: \n{}", ast.debug_string());
        Ok(())
    }

    pub fn result(code: &str, debug_output: bool) -> Int {
        let _ = pretty_env_logger::init();

        if debug_output {
            print_program_debug(code).unwrap();
        }
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
        ($( $code:expr );+ => $out:expr, debug_parse=$debug:expr) => {{
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            assert_eq!(util::result(&code, $debug), $out);
        }};
        ($( $code:expr );+ => $out:expr) => {{
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            assert_eq!(util::result(&code, true), $out);
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
mod fitness {
    use super::*;

    #[test]
    fn long_program() {
        assert_program!("var init";
                        "fun inc()";
                        &"    init += 1\n".repeat(10000);
                        "inc()";
                        "init" => 10000, debug_parse = false); // debug parse output is 10000 lines
    }

    #[test]
    fn recursive_overflow() {
        assert_program!("fun rec()";
                        "    rec()";
                        "rec()" =>X "stack overflow");
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
    fn function_arg() {
        assert_program!("fun plus_one(a_number)";
                        "    return a_number + 1";
                        "plus_one(12)" => 13);
    }

    #[test]
    fn function_return_last_line() {
        assert_program!("fun double(n)";
                        "    n * 2";
                        "double(12)" => 24);
    }

    #[test]
    fn function_complex_args() {
        assert_program!("var x = 12";
                        "var always_add = 12";
                        "fun plus_2(x)";
                        "    x + 2";
                        "fun sum(x, y, z)";
                        "    always_add + x + y + z";
                        "sum(1, x, plus_2(always_add))" => 39);
    }

    #[test]
    fn recursive_function() {
        assert_program!("fun fib(n)";
                        "    if n < 3";
                        "        return 1";
                        "    fib(n-1) + fib(n-2)";
                        "fib(12)" => 144);
    }

    #[test]
    fn dot_call_style() {
        assert_program!("fun is_even(num)";
                        "    num % 2 is 0";
                        "12.is_even()" => 1);

        assert_program!("fun divisible_by(num, divisor)";
                        "    num % divisor is 0";
                        "18.divisible_by(6)" => 1);
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
