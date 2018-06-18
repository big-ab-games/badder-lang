#![allow(unknown_lints)]
#![warn(clippy)]

#[macro_use]
extern crate log;
extern crate indexmap;
extern crate single_value_channel;
extern crate string_cache;
extern crate strsim;

mod common;
mod lexer;
mod parser;
pub mod controller;

use lexer::Token::*;
use std::cmp::*;
use indexmap::IndexMap;
use std::collections::HashSet;
use std::fmt;
use std::iter::Iterator;
use std::mem;
use std::sync::Arc;
use std::{usize, i32};
use common::*;
use strsim::damerau_levenshtein as str_dist;

pub use lexer::Token;
pub use parser::{Ast, Parser};
pub use common::{BadderError, SourceRef};

pub type Int = i32;

const MAX_STACK: usize = 50;
const UNKNOWN_SRC_REF: SourceRef = SourceRef((0, 0), (0, 0));

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum Builtin {
    Size,
    SeqAdd,
    SeqRemove,
}

#[derive(Clone, Hash, PartialEq)]
pub enum FrameData {
    Value(Int, SourceRef),
    /// Callable(args, block)
    Callable(Vec<Token>, Arc<Ast>, SourceRef),
    /// Callables built into the interpreter
    BuiltinCallable(Builtin),
    /// External function managed by the overseer
    ExternalCallable,
    ///Ref(frame_index, id)
    Ref(usize, Token),
    /// Sequence of values
    Sequence(Vec<Int>, SourceRef),
    LoopMarker,
}

impl fmt::Debug for FrameData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value(x, ..) => write!(f, "Value({})", x),
            Callable(..) => write!(f, "Callable"),
            BuiltinCallable(..) => write!(f, "BuiltinCallable"),
            ExternalCallable => write!(f, "ExternalCallable"),
            Sequence(ref vals, ..) => write!(f, "Sequence({:?})", vals),
            Ref(i, ref t) => write!(f, "Ref([{}]:{:?})", i, t),
            LoopMarker => write!(f, "LoopMarker"),
        }
    }
}

impl FrameData {
    fn desc(&self, id: &Token) -> String {
        match *self {
            Value(..) => format!("var {:?}", id),
            Callable(ref args, ..) => format!(
                "{:?}({})",
                id,
                args.iter()
                    .map(|a| format!("{:?}", a))
                    .fold(String::new(), |all, n| all + &n)
            ),
            Sequence(..) | BuiltinCallable(..) => format!("{:?}", id),
            Ref(..) => format!("ref->{:?}", id),
            _ => "_".into(),
        }
    }

    fn add_builtins_to(frame: &mut IndexMap<Token, FrameData>) {
        frame.insert(Id("size(s)".into()), BuiltinCallable(Builtin::Size));
        frame.insert(Id("add(sv)".into()), BuiltinCallable(Builtin::SeqAdd));
        frame.insert(Id("remove(sv)".into()), BuiltinCallable(Builtin::SeqRemove));
    }
}

#[derive(Debug)]
enum InterpreterUpFlow {
    Error(BadderError),
    LoopBreak,
    LoopContinue,
    FunReturn(Int),
}

#[derive(Clone, Copy, Default)]
pub struct StackKey {
    access_up_to: usize,
    access_from: usize,
}

impl fmt::Debug for StackKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.access_from == self.access_up_to || self.access_from == self.access_up_to + 1 {
            write!(f, "all-frames")
        }
        else if self.access_up_to + 2 == self.access_from {
            write!(f, "not-frame({})", self.access_up_to + 1)
        }
        else {
            write!(
                f,
                "not-frame({}-{})",
                self.access_up_to + 1,
                self.access_from - 1
            )
        }
    }
}

impl StackKey {
    fn from_fun_call(fun_declared_frame: usize, call_frame: usize) -> StackKey {
        StackKey {
            access_up_to: fun_declared_frame,
            access_from: call_frame + 1,
        }
    }

    fn can_access(&self, frame_index: usize) -> bool {
        frame_index <= self.access_up_to || frame_index >= self.access_from
    }
}

use FrameData::*;
use InterpreterUpFlow::*;

pub trait Overseer {
    fn oversee(
        &mut self,
        stack: &[IndexMap<Token, FrameData>],
        ast: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<(), ()>;

    fn oversee_after(&mut self, _stack: &[IndexMap<Token, FrameData>], _ast: &Ast) {}

    fn external_function_signatures(&self) -> &[Token];

    fn call_external_function(&mut self, id: Token, args: Vec<Int>) -> Result<Int, String>;
}

pub struct NoOverseer;

impl Overseer for NoOverseer {
    fn oversee(
        &mut self,
        _stack: &[IndexMap<Token, FrameData>],
        _: &Ast,
        _: usize,
        _: StackKey,
    ) -> Result<(), ()> {
        Ok(())
    }

    fn external_function_signatures(&self) -> &[Token] {
        &[]
    }

    fn call_external_function(&mut self, id: Token, _: Vec<Int>) -> Result<Int, String> {
        Err(format!("Unknown external function {:?}", id))
    }
}

#[derive(Debug)]
pub struct Interpreter<O: Overseer> {
    stack: Vec<IndexMap<Token, FrameData>>,
    overseer: O,
}

#[inline]
fn bool_to_num(b: bool) -> Int {
    if b {
        1
    }
    else {
        0
    }
}

fn parent_error<T, S: Into<String>>(desc: S) -> Result<T, InterpreterUpFlow> {
    Err(Error(
        BadderError::at(UNKNOWN_SRC_REF).describe(Stage::Interpreter, desc.into()),
    ))
}

fn convert_signed_index(mut i: i32, length: usize) -> usize {
    if length == 0 {
        return max(i, 0) as usize;
    }

    let len = length as i32;
    if i < 0 {
        i = (len + i % len) % len;
    }
    i as usize
}

impl Default for Interpreter<NoOverseer> {
    fn default() -> Interpreter<NoOverseer> {
        Interpreter::new(NoOverseer)
    }
}

impl<O: Overseer> Interpreter<O> {
    pub fn new(overseer: O) -> Interpreter<O> {
        let mut init_frame = IndexMap::new();
        for ext_fun in overseer.external_function_signatures() {
            init_frame.insert(ext_fun.clone(), FrameData::ExternalCallable);
        }
        FrameData::add_builtins_to(&mut init_frame);
        Interpreter {
            stack: vec![init_frame],
            overseer,
        }
    }

    fn unknown_id_err(&mut self, id: &Token, stack_key: StackKey) -> String {
        let keys: HashSet<&Token> = self.stack
            .iter()
            .enumerate()
            .filter(|&(i, _)| stack_key.can_access(i))
            .flat_map(|(_, m)| m.keys())
            .collect();
        if keys.is_empty() {
            format!("id `{:?}` not found in scope", id)
        }
        else {
            assert!(
                !keys.contains(id),
                "!keys.contains(id): `{:?}` is available",
                id
            );
            let mut available: Vec<_> = keys.iter()
                .map(|k| k.id_str().unwrap_or("#"))
                .filter(|k| !k.starts_with('#'))
                .collect();

            available.sort_unstable(); // ensure consistent order (after hashsetting)

            if let Some(id) = id.id_str() {
                // sort by string simularity
                available.sort_by_key(|known| str_dist(id, known));
            }
            let available = available.join(", ");
            format!(
                "id `{:?}` not found in scope, available: {{{}}}",
                id, available
            )
        }
    }

    fn highest_frame_idx(
        &self,
        key: &Token,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Option<usize> {
        if self.stack.is_empty() {
            return None;
        }

        let mut idx = min(self.stack.len() - 1, current_scope);
        loop {
            if stack_key.can_access(idx) && self.stack.len() > idx
                && self.stack[idx].contains_key(key)
            {
                return Some(idx);
            }
            if idx == 0 {
                break;
            }
            idx -= 1;
        }
        None
    }

    #[inline]
    fn log_eval(&mut self, ast: &Ast, current_scope: usize, stack_key: StackKey) {
        match *ast {
            Ast::Line(..) | Ast::LinePair(..) | Ast::Num(..) => (),
            _ => trace!(
                "base: {}, access: {:?}\nstack: {:?}\neval({:?})",
                current_scope,
                stack_key,
                self.stack,
                ast
            ),
        }
    }

    #[inline]
    fn eval_bin_op(
        &mut self,
        token: &Token,
        left: &Ast,
        right: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval { ($expr:expr) => { self.eval($expr, current_scope, stack_key) } }
        macro_rules! unwrap_checked {
            ($c:expr) => { $c.map(Ok).unwrap_or_else(|| parent_error("overflow")) }
        }

        match *token {
            Pls => unwrap_checked!(eval!(left)?.checked_add(eval!(right)?)),
            Sub => unwrap_checked!(eval!(left)?.checked_sub(eval!(right)?)),
            Mul => unwrap_checked!(eval!(left)?.checked_mul(eval!(right)?)),
            Mod | Div => match (eval!(right)?, token) {
                (0, _) => parent_error("Cannot divide by zero"),
                (divisor, &Div) => unwrap_checked!(eval!(left)?.checked_div(divisor)),
                (divisor, _) => unwrap_checked!(eval!(left)?.checked_rem(divisor)),
            },
            And => match eval!(left)? {
                0 => Ok(0),
                _ => eval!(right),
            },
            Or => match eval!(left)? {
                0 => eval!(right),
                x => Ok(x),
            },
            Is => Ok(bool_to_num(eval!(left)? == eval!(right)?)),
            Gt => Ok(bool_to_num(eval!(left)? > eval!(right)?)),
            Lt => Ok(bool_to_num(eval!(left)? < eval!(right)?)),
            GtEq => Ok(bool_to_num(eval!(left)? >= eval!(right)?)),
            LtEq => Ok(bool_to_num(eval!(left)? <= eval!(right)?)),
            _ => parent_error(format!("Unexpected BinOp token `{:?}`", token)),
        }
    }

    #[inline]
    fn eval_while(
        &mut self,
        expr: &Ast,
        block: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval {($expr:expr) => { self.eval($expr, current_scope, stack_key) }}

        if eval!(expr)? == 0 {
            return Ok(0);
        }
        let loop_token = Id("#loop".into());
        self.stack[current_scope].insert(loop_token.clone(), LoopMarker);
        loop {
            self.stack.push(IndexMap::new());
            let eval = self.eval(block, current_scope + 1, stack_key);
            self.stack.pop();
            match eval {
                Err(LoopBreak) => break,
                Ok(_) | Err(LoopContinue) => (),
                err @ Err(_) => return err,
            };
            if eval!(expr)? == 0 {
                break;
            }
        }
        self.stack[current_scope].remove(&loop_token);
        Ok(0)
    }

    #[allow(too_many_arguments)]
    #[inline]
    fn eval_for_in(
        &mut self,
        idx_id: &Option<Token>,
        item_id: &Token,
        list_expr: &Ast,
        block: &Ast,
        src: SourceRef,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval_seq {($expr:expr) => { self.eval_seq($expr, current_scope, stack_key) }}

        let mut list = eval_seq!(list_expr)?;
        if list.is_empty() {
            return Ok(0);
        }
        let loop_token = Id("#loop".into());
        self.stack[current_scope].insert(loop_token.clone(), LoopMarker);
        let mut index = 0;
        while index < list.len() {
            let mut frame = IndexMap::new();
            if let Some(ref id) = *idx_id {
                frame.insert(id.clone(), Value(index as i32, src));
            }
            frame.insert(item_id.clone(), Value(list[index], src));
            self.stack.push(frame);
            let eval = self.eval(block, current_scope + 1, stack_key);
            self.stack.pop();
            match eval {
                Err(LoopBreak) => break,
                Ok(_) | Err(LoopContinue) => (),
                err @ Err(_) => return err,
            };
            index += 1;
            list = eval_seq!(list_expr)?;
        }
        self.stack[current_scope].remove(&loop_token);
        Ok(0)
    }

    #[inline]
    fn eval_fun_call(
        &mut self,
        id: &Token,
        args: &[Ast],
        current_scope: usize,
        stack_key: StackKey,
        // Parent oversee values deferred until after
        // function arguments have been overseen
        deferred_oversee: (&Ast, usize, StackKey),
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval {($expr:expr) => { self.eval($expr, current_scope, stack_key) }}
        macro_rules! eval_seq {($expr:expr) => { self.eval_seq($expr, current_scope, stack_key) }}
        macro_rules! highest_frame_idx {
            ($index:expr) => { self.highest_frame_idx($index, current_scope, stack_key) }
        }
        macro_rules! oversee_deferred {
            ($deferred_oversee:expr) => {{
                let (defer_ast, defer_scope, defer_key) = $deferred_oversee;
                if self.overseer
                    .oversee(&self.stack, defer_ast, defer_scope, defer_key)
                    .is_err()
                {
                    return Err(Error(
                        BadderError::at(defer_ast.src())
                            .describe(Stage::Interpreter, "cancelled"),
                    ));
                }
            }}
        }

        if let Some(idx) = highest_frame_idx!(id) {
            let mut builtin_callable = None;
            if let BuiltinCallable(builtin) = self.stack[idx][id] {
                builtin_callable = Some(builtin);
            }
            if let Some(builtin) = builtin_callable {
                return self.call_builtin(builtin, args, current_scope, stack_key, deferred_oversee);
            }

            if self.stack[idx][id] == ExternalCallable {
                let mut evaluated_args = vec![];
                for arg in args {
                    evaluated_args.push(eval!(arg)?);
                }

                oversee_deferred!(deferred_oversee);

                return self.call_external(id.clone(), evaluated_args);
            }

            let (mut arg_ids, callable_block, src) = {
                match self.stack[idx][id] {
                    Callable(ref arg_ids, ref block, src) => {
                        (arg_ids.clone(), Arc::clone(block), src)
                    }
                    _ => {
                        return parent_error(format!("Invalid reference to non callable `{:?}`", id))
                    }
                }
            };

            // construct new function call stack frame
            let mut f_frame = IndexMap::new();
            for i in 0..args.len() {
                let data = match args[i] {
                    ref a @ Ast::Seq(..) => Sequence(eval_seq!(a)?, src),
                    Ast::ReferSeq(ref id, ..) => {
                        if let Some(idx) = highest_frame_idx!(id) {
                            Ref(idx, id.clone())
                        }
                        else {
                            return parent_error(self.unknown_id_err(id, stack_key));
                        }
                    }
                    ref ast => Value(eval!(ast)?, src),
                };
                f_frame.insert(mem::replace(&mut arg_ids[i], Eol), data);
            }
            oversee_deferred!(deferred_oversee);
            self.stack.push(f_frame);

            let out = match self.eval(
                &callable_block,
                current_scope + 1,
                StackKey::from_fun_call(idx, current_scope),
            ) {
                Err(FunReturn(value)) => Ok(value),
                Ok(x) => Ok(x),
                x => x,
            };

            // clean stack
            self.stack.pop();
            return out;
        }
        else {
            parent_error(self.unknown_id_err(id, stack_key))
        }
    }

    #[inline]
    fn eval_refer_seq_index(
        &mut self,
        seq_id: &Token,
        index_expr: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval {($expr:expr) => { self.eval($expr, current_scope, stack_key) }}

        if let Some(idx) = self.highest_frame_idx(seq_id, current_scope, stack_key) {
            let (mut idx, mut seq_id) = (idx, seq_id.clone());
            while let Ref(n_idx, ref n_id) = self.stack[idx][&seq_id] {
                idx = n_idx;
                seq_id = n_id.clone();
            }

            let seq_len = match self.stack[idx][&seq_id] {
                Sequence(ref v, ..) => Ok(v.len()),
                ref data => parent_error(format!(
                    "Invalid sequence index reference to non-sequence `{}`",
                    data.desc(&seq_id)
                )),
            }?;
            let actual_index = eval!(index_expr)?;
            let index = convert_signed_index(actual_index, seq_len);
            if seq_len == 0 {
                return parent_error(format!(
                    "Invalid sequence index {} not in empty sequence",
                    actual_index
                ));
            }
            else if seq_len as usize <= index {
                return parent_error(format!(
                    "Invalid sequence index {} not in 0..{} (or negative)",
                    index, seq_len
                ));
            }
            Ok(match self.stack[idx][&seq_id] {
                Sequence(ref vec, ..) => vec[index],
                _ => unreachable!(),
            })
        }
        else {
            parent_error(self.unknown_id_err(seq_id, stack_key))
        }
    }

    #[inline]
    fn eval_reassign_seq_index(
        &mut self,
        seq_id: &Token,
        index_expr: &Ast,
        expr: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval {($expr:expr) => { self.eval($expr, current_scope, stack_key) }}

        if let Some(idx) = self.highest_frame_idx(seq_id, current_scope, stack_key) {
            let (mut idx, mut seq_id) = (idx, seq_id.clone());
            while let Ref(n_idx, ref n_id) = self.stack[idx][&seq_id] {
                idx = n_idx;
                seq_id = n_id.clone();
            }

            let seq_len = match self.stack[idx][&seq_id] {
                Sequence(ref v, ..) => Ok(v.len()),
                ref data => parent_error(format!(
                    "Invalid sequence index reassignment to non-sequence `{}`",
                    data.desc(&seq_id)
                )),
            }?;
            let index = convert_signed_index(eval!(index_expr)?, seq_len);
            if seq_len as usize <= index {
                return parent_error(format!(
                    "Invalid sequence index {} not in 0..{} (or negative)",
                    index, seq_len
                ));
            }
            let new_val = eval!(expr)?;
            match self.stack[idx].get_mut(&seq_id) {
                Some(&mut Sequence(ref mut vec, ..)) => vec[index] = new_val,
                _ => unreachable!(),
            }
            Ok(0)
        }
        else {
            parent_error(self.unknown_id_err(seq_id, stack_key))
        }
    }

    /// Evaluates the passed in syntax tree
    /// :current_scope the stack frame index currently running in
    /// :restrict_ref optional max frame index referencable
    ///    (not including >= current_scope, which is always ok)
    fn eval(
        &mut self,
        ast: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Int, InterpreterUpFlow> {
        macro_rules! eval { ($expr:expr) => { self.eval($expr, current_scope, stack_key) } }
        macro_rules! eval_seq {($expr:expr) => { self.eval_seq($expr, current_scope, stack_key) }}
        macro_rules! highest_frame_idx {
            ($index:expr) => { self.highest_frame_idx($index, current_scope, stack_key) };
        }

        self.log_eval(ast, current_scope, stack_key);

        let mut deferred_oversee = None;
        if let Ast::Call(..) = *ast {
            deferred_oversee = Some((ast, current_scope, stack_key))
        }
        else if self.overseer
            .oversee(&self.stack, ast, current_scope, stack_key)
            .is_err()
        {
            return Err(Error(
                BadderError::at(ast.src()).describe(Stage::Interpreter, "cancelled"),
            ));
        }

        let result = match *ast {
            Ast::Num(Num(x), ..) => Ok(x),
            Ast::BinOp(ref token, ref left, ref right, ..) => {
                self.eval_bin_op(token, left, right, current_scope, stack_key)
            }
            Ast::LeftUnaryOp(Sub, ref val, ..) => Ok(-eval!(val)?),
            Ast::LeftUnaryOp(Not, ref val, ..) => Ok(match eval!(val)? {
                0 => 1,
                _ => 0,
            }),
            Ast::Assign(ref id, ref expr, src) => {
                let v = eval!(expr)?;
                self.stack[current_scope].insert(id.clone(), Value(v, src));
                Ok(v)
            }
            Ast::Reassign(ref id, ref expr, ..) => {
                // reassign to any parent scope
                if let Some(idx) = highest_frame_idx!(id) {
                    let v = eval!(expr)?;
                    let ass_src = match self.stack[idx][id] {
                        Value(.., src) => src,
                        _ => unreachable!(),
                    };
                    *self.stack[idx].get_mut(id).unwrap() = Value(v, ass_src);
                    Ok(v)
                }
                else {
                    parent_error(format!(
                        "{}, or did you mean `var {:?} =`?",
                        self.unknown_id_err(id, stack_key),
                        id
                    ))
                }
            }
            Ast::Refer(ref id, ..) => {
                if let Some(idx) = highest_frame_idx!(id) {
                    match self.stack[idx][id] {
                        Value(v, ..) => Ok(v),
                        _ => parent_error(format!("Invalid reference to non number `{:?}`", id)),
                    }
                }
                else {
                    parent_error(self.unknown_id_err(id, stack_key))
                }
            }
            Ast::If(ref expr, ref block, ref else_line, ..) => Ok(match eval!(expr)? {
                0 => match *else_line {
                    Some(ref else_line) => eval!(else_line)?,
                    None => 0,
                },
                _ => {
                    self.stack.push(IndexMap::new());
                    let eval = self.eval(block, current_scope + 1, stack_key);
                    self.stack.pop();
                    eval?;
                    0
                }
            }),
            Ast::While(ref expr, ref block, ..) => {
                self.eval_while(expr, block, current_scope, stack_key)
            }
            Ast::ForIn(ref idx_id, ref item_id, ref list_expr, ref block, src) => {
                self.eval_for_in(
                    idx_id,
                    item_id,
                    list_expr,
                    block,
                    src,
                    current_scope,
                    stack_key,
                )
            }
            Ast::LoopNav(ref token, ..) => {
                let loop_token = Id("#loop".into());
                if highest_frame_idx!(&loop_token).is_some() {
                    match *token {
                        Break => Err(LoopBreak),
                        Continue => Err(LoopContinue),
                        _ => parent_error(format!("Unknown loop nav `{:?}`", token)),
                    }
                }
                else {
                    parent_error(format!("Invalid use of loop nav `{:?}`", token))
                }
            }
            Ast::AssignFun(ref id, ref args, ref block, src) => {
                let top = self.stack.len() - 1;
                match self.stack[current_scope].get(id) {
                    None | Some(&Callable(..)) => (), // overwrite
                    Some(other) => {
                        let desc = format!(
                            "Declaration `fun {:?}` conflicts with `{}` in same scope",
                            id,
                            other.desc(id)
                        );
                        return Err(Error(
                            BadderError::at(ast.src()).describe(Stage::Interpreter, desc),
                        ));
                    }
                };
                self.stack[top].insert(id.clone(), Callable(args.clone(), Arc::clone(block), src));
                Ok(0)
            }
            Ast::Call(ref id, ref args, ..) => {
                self.eval_fun_call(id, args, current_scope, stack_key, deferred_oversee.unwrap())
            }
            Ast::Return(ref expr, ..) => Err(FunReturn(eval!(expr)?)),
            Ast::ReferSeqIndex(ref seq_id, ref index_expr, ..) => {
                self.eval_refer_seq_index(seq_id, index_expr, current_scope, stack_key)
            }
            Ast::ReassignSeqIndex(ref seq_id, ref index_expr, ref expr, ..) => {
                self.eval_reassign_seq_index(seq_id, index_expr, expr, current_scope, stack_key)
            }
            Ast::AssignSeq(ref id, ref list, src) => {
                let v = eval_seq!(list)?;
                match self.stack[current_scope].get(id) {
                    None | Some(&Sequence(..)) => (), // overwrite
                    Some(other) => {
                        let desc = format!(
                            "Assignment of `seq {:?}[]` conflicts with `{}` in same scope",
                            id,
                            other.desc(id)
                        );
                        return Err(Error(
                            BadderError::at(ast.src()).describe(Stage::Interpreter, desc),
                        ));
                    }
                };
                self.stack[current_scope].insert(id.clone(), Sequence(v, src));
                Ok(0)
            }
            Ast::Line(scope, ref expr, ..) => {
                let scope = max(current_scope, scope);
                if scope > MAX_STACK {
                    return parent_error("stack overflow");
                }
                self.eval(expr, scope, stack_key)
            }
            Ast::LinePair(ref line, ref next_line, ..) => {
                eval!(line)?;
                let mut next = next_line;
                while let Ast::LinePair(ref l2, ref l3, ..) = **next {
                    eval!(l2)?;
                    next = l3;
                }
                eval!(next)
            }
            Ast::Empty(..) => Ok(0),
            _ => parent_error(format!("Unexpected syntax {:?}", ast)),
        };

        self.overseer.oversee_after(&self.stack, ast);

        match result {
            Err(Error(BadderError {
                stage,
                description,
                src,
            })) => {
                if src == UNKNOWN_SRC_REF {
                    Err(Error(
                        BadderError::at(ast.src()).describe(stage, description),
                    ))
                }
                else {
                    Err(Error(BadderError::at(src).describe(stage, description)))
                }
            }
            x => x,
        }
    }

    fn eval_seq(
        &mut self,
        list: &Ast,
        current_scope: usize,
        stack_key: StackKey,
    ) -> Result<Vec<Int>, InterpreterUpFlow> {
        macro_rules! eval {($expr:expr) => { self.eval($expr, current_scope, stack_key) }}

        let result = match *list {
            Ast::Seq(ref exprs, ..) => {
                let mut evals = vec![];
                for ex in exprs {
                    evals.push(eval!(ex)?);
                }
                Ok(evals)
            }
            Ast::ReferSeq(ref id, ..) => {
                if let Some(idx) = self.highest_frame_idx(id, current_scope, stack_key) {
                    let (mut idx, mut id) = (idx, id.clone());
                    while let Ref(n_idx, ref n_id) = self.stack[idx][&id] {
                        idx = n_idx;
                        id = n_id.clone();
                    }

                    match self.stack[idx][&id] {
                        Sequence(ref v, ..) => Ok(v.clone()),
                        ref data => parent_error(format!(
                            "Invalid sequence referal to non-sequence `{}`",
                            data.desc(&id)
                        )),
                    }
                }
                else {
                    parent_error(self.unknown_id_err(id, stack_key))
                }
            }
            _ => parent_error(format!("Unexpected Seq syntax {:?}", list)),
        };

        match result {
            Err(Error(BadderError {
                stage,
                description,
                src,
            })) => {
                if src == UNKNOWN_SRC_REF {
                    Err(Error(
                        BadderError::at(list.src()).describe(stage, description),
                    ))
                }
                else {
                    Err(Error(BadderError::at(src).describe(stage, description)))
                }
            }
            x => x,
        }
    }

    fn call_builtin(
        &mut self,
        builtin: Builtin,
        args: &[Ast],
        current_scope: usize,
        stack_key: StackKey,
        // Parent oversee values deferred until after
        // function arguments have been overseen
        deferred_oversee: (&Ast, usize, StackKey),
    ) -> Result<Int, InterpreterUpFlow> {
        let arg1 = if args.len() == 2 {
            Some(self.eval(&args[1], current_scope, stack_key)?)
        }
        else {
            None
        };

        // should be enough to defer function oversee to here
        let (defer_ast, defer_scope, defer_key) = deferred_oversee;
        if self.overseer
            .oversee(&self.stack, defer_ast, defer_scope, defer_key)
            .is_err()
        {
            return Err(Error(
                BadderError::at(defer_ast.src())
                    .describe(Stage::Interpreter, "cancelled"),
            ));
        }

        // all builtins are core lib for seq, ie first arg is a seq
        match args[0] {
            Ast::ReferSeq(ref id, ..) => {
                if let Some(idx) = self.highest_frame_idx(id, current_scope, stack_key) {
                    let (mut idx, mut id) = (idx, id.clone());
                    while let Ref(n_idx, ref n_id) = self.stack[idx][&id] {
                        idx = n_idx;
                        id = n_id.clone();
                    }
                    match self.stack[idx].get_mut(&id) {
                        Some(&mut Sequence(ref mut v, ..)) => match builtin {
                            Builtin::Size => Ok(v.len() as i32),
                            Builtin::SeqAdd => {
                                if v.len() == (i32::MAX - 1) as usize {
                                    return parent_error("`add(sv)` failed, sequence is max size");
                                }
                                v.push(arg1.unwrap());
                                Ok(0)
                            }
                            Builtin::SeqRemove => {
                                let index = convert_signed_index(arg1.unwrap(), v.len());
                                if v.is_empty() {
                                    return parent_error(format!(
                                        "Invalid sequence index {} not in empty sequence",
                                        arg1.unwrap()
                                    ));
                                }
                                else if v.len() <= index {
                                    return parent_error(format!(
                                        "Invalid sequence index {} not in 0..{} (or negative)",
                                        index,
                                        v.len()
                                    ));
                                }
                                Ok(v.remove(index))
                            }
                        },
                        Some(ref data) => parent_error(format!(
                            "Invalid sequence referal to non-sequence `{}`",
                            data.desc(&id)
                        )),
                        None => unreachable!(),
                    }
                }
                else {
                    parent_error(self.unknown_id_err(id, stack_key))
                }
            }
            // otherwise, ie literal, just evaluate it
            ref ast => {
                let literal = self.eval_seq(ast, current_scope, stack_key)?;
                match builtin {
                    Builtin::Size => Ok(literal.len() as i32),
                    Builtin::SeqAdd => Ok(0),
                    Builtin::SeqRemove => {
                        let index = convert_signed_index(arg1.unwrap(), literal.len());
                        Ok(literal[index])
                    }
                }
            }
        }
    }

    /// Note: currently external functions assume num only arguments
    fn call_external(&mut self, id: Token, args: Vec<Int>) -> Result<Int, InterpreterUpFlow> {
        match self.overseer.call_external_function(id, args) {
            Ok(result) => Ok(result),
            Err(desc) => parent_error(desc),
        }
    }

    pub fn evaluate(&mut self, ast: &Ast) -> Res<Int> {
        match self.eval(ast, 0, StackKey::default()) {
            Ok(x) => Ok(x),
            Err(Error(desc)) => Err(desc),
            Err(err) => panic!(err),
        }
    }
}

#[cfg(test)]
#[macro_use]
mod util {
    extern crate env_logger;

    use super::*;
    use std::sync::mpsc;
    use std::thread;
    use std::time::{Duration, Instant};

    fn eval_within(code: &str, timeout: Duration) -> Res<Int> {
        let (sender, receiver) = mpsc::channel();
        let before_parse = Instant::now();
        debug!("parsing...");
        let code: Ast = Parser::parse_str(code)?;
        debug!("parsed in {:?}, interpreting...", before_parse.elapsed());

        thread::spawn(move || {
            let before_interp = Instant::now();
            sender.send(Interpreter::default().evaluate(&code)).unwrap();
            debug!("interpreted in {:?}", before_interp.elapsed());
        });

        let now = Instant::now();
        while now.elapsed() < timeout {
            match receiver.try_recv() {
                Ok(res) => return res,
                _ => thread::sleep(Duration::from_millis(5)),
            }
        }
        Err(BadderError::at(SourceRef((0, 0),(0, 0))) // TODO
            .describe(Stage::Interpreter, format!("Program did not return within {:?}", timeout)))
    }

    fn print_program_debug(code: &str) -> Res<()> {
        let ast = Parser::parse_str(code)?;
        debug!("Program: \n{}", ast.debug_string());
        Ok(())
    }

    pub fn result(code: &str, debug_output: bool) -> Int {
        let _ = env_logger::try_init();

        if debug_output {
            print_program_debug(code).unwrap();
        }
        eval_within(code, Duration::from_secs(1)).unwrap()
    }

    pub fn error(code: &str) -> BadderError {
        let _ = env_logger::try_init();

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

    /// `assert_program!("..." => 123)`
    /// Assert that a program outputs a given result
    /// `assert_program!("..." =>X "Three dots are not a program", "you are not trying")`
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
        ($( $code:expr );+ =>X $( $sub:expr ),+) => {
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            let err = util::error(&code);
            let err_lower = err.description.to_lowercase();
            $(
                let substring_lower = $sub.to_lowercase();
                assert!(err_lower.contains(substring_lower.as_str()),
                    format!("Substring:`{}` not in error: `{:?}`", $sub, err));
            )+
        };
        ($( $code:expr );+ =>X $( $sub:expr ),+; src = $src_ref:expr ) => {
            let mut code = String::new();
            $(
                code = code + $code + "\n";
            )+
            let err = util::error(&code);
            let err_lower = err.description.to_lowercase();
            $(
                let substring_lower = $sub.to_lowercase();
                assert!(err_lower.contains(substring_lower.as_str()),
                    format!("Substring:`{}` not in error: `{:?}`", $sub, err));
            )+
            assert_eq!(err.src, $src_ref);
        };
    }
}

// reproductions of encountered issues/bugs
#[cfg(test)]
mod issues {
    use super::*;

    #[test]
    fn variable_in_funtion_loop() {
        assert_program!("fun some_func()";
                        "    var count";
                        "    for i in 1,2,3";
                        "        count += i";
                        "    count";
                        "loop";
                        "    if 1";
                        "        some_func()";
                        "    break";
                        "1" => 1);
    }

    #[test]
    fn if_else_if_else_never_hitting_else() {
        assert_program!(
            "var x = 123";
            "var y = 234";
            "if x is 123 and y is 123";
            "    # bunch of stuff";
            "    if 1";
            "        var z = x + y";
            "        call_some_function()";
            "else if y is not 234";
            "    # more stuff";
            "    var z";
            "    if 1 - 1";
            "        z = x - 2 * y";
            "    else";
            "        z = x - y";
            "    call_another_func()";
            "else ";
            "    x = 3";
            "    y = 5";
            "x + y" => 8);
    }
}

#[cfg(test)]
mod fitness {
    use super::*;

    #[test]
    fn long_program() {
        assert_program!("var init";
                        "fun inc()";
                        &"    init += 1\n".repeat(10_000);
                        "inc()";
                        "init" => 10_000, debug_parse = false); // debug parse output is 10000 lines
    }

    #[test]
    fn recursive_overflow() {
        assert_program!("fun rec()";
                        "    rec()";
                        "rec()" =>X "stack overflow"; src = SourceRef((2,5), (2,10)));
    }
}

#[cfg(test)]
mod core_lib {
    use super::*;

    #[test]
    fn seq_size() {
        assert_program!("seq nums[] = 1,2,3,4,5,4,3,2,1";
                        "size(nums[])"; // simple call style
                        "nums[].size()" => 9);
    }

    #[test]
    fn seq_size_literal() {
        assert_program!("size((5,4,3,2))" => 4);
    }

    #[test]
    fn seq_remove() {
        assert_program!("seq nums[] = 1,2,3";
                        "remove(nums[], 1)"; // simple call style
                        "nums[].remove(0)";
                        "nums[].size()" => 1);
    }

    #[test]
    fn seq_remove_return_removed() {
        assert_program!("seq nums[] = 1,2,3";
                        "nums[].remove(0)" => 1);
    }

    #[test]
    fn seq_remove_literal() {
        assert_program!("remove((5,4,3,2), 2)" => 3);
    }

    #[test]
    fn seq_remove_neg_index() {
        assert_program!("seq nums[] = 1,2,3";
                        "nums[].remove(-1)"; // should remove size-1 index
                        "var last";
                        "for n in nums[]";
                        "    last = n";
                        "last" => 2);
    }

    #[test]
    fn seq_add() {
        assert_program!("seq nums[] = 1,2";
                        "add(nums[], 4999)"; // simple call style
                        "nums[].add(5000)";
                        "nums[3]" => 5000);
    }

    #[test]
    fn seq_add_literal() {
        assert_program!("add((5,4,3,2), 5000)" => 0);
    }
}

#[cfg(test)]
mod list_functions {
    use super::*;

    #[test]
    fn def_seq_function() {
        assert_program!("fun count_number_of(list[], num)";
                        "    var count";
                        "    for n in list[]";
                        "        if n is num";
                        "            count += 1";
                        "    count";
                        "seq fib[] = 1,1,2,3,5";
                        "count_number_of(fib[], 1)" => 2);
    }

    #[test]
    fn pass_seq_by_reference() {
        assert_program!("fun overwrite_with(list[], num)";
                        "    for i, n in list[]";
                        "        list[i] = num";
                        "seq fib[] = 1,1,2,3,5";
                        "overwrite_with(fib[], 4)";
                        "fib[3]" => 4);
    }

    #[test]
    fn dot_call_syntax() {
        assert_program!("fun first(list[])";
                        "    list[0]";
                        "seq fib[] = 1,1,2,3,5";
                        "fib[].first()" => 1);

        assert_program!("fun last_plus(list[], num)";
                        "    var last";
                        "    for n in list[]";
                        "        last = n";
                        "    last + num";
                        "seq fib[] = 1,1,2,3,5";
                        "fib[].last_plus(3)" => 8);
    }
}

#[cfg(test)]
mod lists {
    use super::*;

    #[test]
    fn init() {
        assert_program!("seq nums[]" => 0);
        assert_program!("seq nums[] = 1,2,3" => 0);
        assert_program!("var x = 2";
                        "fun a()";
                        "    123";
                        "seq nums[] = x, a(), 3" => 0);
    }

    #[test]
    fn index_access() {
        assert_program!("seq nums[] = 1,2,3";
                        "nums[1]" => 2);
    }

    #[test]
    fn negative_index_access() {
        assert_program!("seq nums[] = 1,2,3";
                        "nums[-1]" => 3);
        assert_program!("seq nums[] = 1,2,3";
                        "nums[-3]" => 1);
        assert_program!("seq nums[] = 1,2,3";
                        "nums[-13]" => 3);
    }

    #[test]
    fn index_reassignment() {
        assert_program!("seq nums[] = 1,2,3";
                        "nums[0] = 435";
                        "nums[0]" => 435);
    }

    #[test]
    fn manual_iteration() {
        assert_program!("seq nums[] = 1,1,2,3,5";
                        "var sum";
                        "var i";
                        "while i < 5";
                        "    sum += nums[i]";
                        "    i += 1";
                        "sum" => 12);
    }

    #[test]
    fn iteration() {
        assert_program!("seq nums[] = 1,1,2,3,5";
                        "var sum";
                        "for num in nums[]";
                        "    sum += num";
                        "sum" => 12);
    }

    #[test]
    fn literal_iteration() {
        assert_program!("var product = 1";
                        "for n in 5,4,3,2,1";
                        "    product *= n";
                        "product" => 120);
    }

    #[test]
    fn enumerated_iteration() {
        assert_program!("seq nums[] = 1,1,2,3,5";
                        "var index_sum";
                        "for index, num in nums[]";
                        "    index_sum += index";
                        "index_sum" => 10);
    }

    #[test]
    fn fib_loop_function() {
        assert_program!("fun fib(n)";
                        "    var its = 1";
                        "    var last";
                        "    var curr = 1";
                        "    while its < n";
                        "        var next = curr + last";
                        "        last = curr";
                        "        curr = next";
                        "        its += 1";
                        "    curr";
                        "fib(12)" => 144);
    }

    #[test]
    fn distinct_signature_from_var() {
        assert_program!("var f = 12";
                        "seq f[] = 13";
                        "f[0]" => 13);
        assert_program!("var f = 12";
                        "seq f[] = 13";
                        "f" => 12);
    }

    #[test]
    fn distinct_signature_from_fun() {
        assert_program!("fun f()";
                        "    12";
                        "seq f[] = 13";
                        "f[0]" => 13);
        assert_program!("fun f()";
                        "    12";
                        "seq f[] = 13";
                        "f()" => 12);
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
                        "            if 4";
                        "                var b = 643";
                        // call in scope 3 should have access to scopes >3 and <=2 (def scope)
                        "                out = a_and_b_p1()"; // should refer to a(scope1), b(scope2)
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
    fn fib_function() {
        // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144
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

    #[test]
    fn dot_chaining() {
        assert_program!("fun double(n)";
                        "    n * 2";
                        "(5.double().double()).double()" => 40);
    }

    #[test]
    fn overwrite_fun_with_fun() {
        assert_program!("fun f()";
                        "    1";
                        "fun f()";
                        "    2";
                        "f()" => 2);
    }

    #[test]
    fn distinct_signature_from_var() {
        assert_program!("var f = 12";
                        "fun f()";
                        "    2";
                        "f()" => 2);
        assert_program!("var f = 12";
                        "fun f()";
                        "    2";
                        "f" => 12);
    }

    #[test]
    fn call_by_signature_same_scope() {
        assert_program!("fun max(a, b)";
                        "    if b > a";
                        "        return b";
                        "    a";
                        "fun max(a, b, c)";
                        "    max(a, max(b, c))";
                        "max(1, 3, 2)" => 3);
    }

    #[test]
    fn call_by_signature_across_scope() {
        assert_program!("fun number(a)";
                        "    a";
                        "var out";
                        "if 1";
                        "    fun number(b, c)";
                        "        b + c";
                        "    out = number(2)";
                        "out" => 2);
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
                        "x" =>X "x", "scope"; src = SourceRef((3,1), (3,2)));
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
                        "x" =>X "indent"; src = SourceRef((3,1), (3,4)));
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
        assert_program!("b = 1" =>X "`b`", "not found"; src = SourceRef((1,1), (1,6)));
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
