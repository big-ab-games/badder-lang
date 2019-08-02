use std::ops::{Range, RangeFrom};
use super::Int;
use crate::{
    common::*,
    lexer::{Token::*, *},
};
use log::trace;
use rustc_hash::FxHashSet;
use std::{borrow::Cow, fmt, rc::Rc, sync::Arc};
use string_cache::DefaultAtom as Atom;

/// Abstract syntax tree
#[derive(PartialEq, Clone, Hash)]
pub enum Ast {
    Num(Token, SourceRef),
    BinOp(Token, Box<Ast>, Box<Ast>, SourceRef),
    LeftUnaryOp(Token, Box<Ast>, SourceRef),
    /// Assign(variable_id, ast)
    Assign(Token, Box<Ast>, SourceRef),
    Reassign(Token, Box<Ast>, SourceRef),
    /// Refer(variable_id)
    Refer(Token, SourceRef),
    /// If(if-expr, resultant-block, Line(If), is_else)
    If(Box<Ast>, Arc<Ast>, Option<Box<Ast>>, bool, SourceRef),
    /// While(if-expr, resultant-block)
    While(Box<Ast>, Arc<Ast>, SourceRef),
    /// ForIn(index-id, item-id, list, block)
    ForIn(Option<Token>, Token, Box<Ast>, Arc<Ast>, SourceRef),
    /// LoopNav(break|continue)
    LoopNav(Token, SourceRef),
    /// AssignFun(function-id, args, block) defines a function
    AssignFun(Token, Vec<Token>, Arc<Ast>, SourceRef),
    // Return(expr) used inside function blocks
    Return(Box<Ast>, SourceRef),
    /// Call(function-id, args)
    Call(Token, Vec<Ast>, SourceRef),
    /// Literal sequence of expressions
    Seq(Vec<Ast>, SourceRef),
    /// AssignSeq(seq-id, Seq)
    AssignSeq(Token, Box<Ast>, SourceRef),
    /// ReferSeq(seq-id) refer to a sequence
    ReferSeq(Token, SourceRef),
    /// ReferSeqIndex(seq-id, index)
    ReferSeqIndex(Token, Box<Ast>, SourceRef),
    /// ReassignSeqIndex(seq-id, index, val)
    ReassignSeqIndex(Token, Box<Ast>, Box<Ast>, SourceRef),
    /// Line(scope, expr)
    Line(usize, Box<Ast>, SourceRef),
    /// LinePair(line, next_line)
    LinePair(Box<Ast>, Box<Ast>, SourceRef),
    Empty(SourceRef),
}

impl fmt::Debug for Ast {
    /// Safe for large recursive structures
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Ast::Num(ref t, ..) => write!(f, "Num({:?})", t),
            Ast::BinOp(ref t, ref l, ref r, ..) => write!(f, "BinOp({:?},{:?},{:?})", t, l, r),
            Ast::LeftUnaryOp(ref t, ref expr, ..) => write!(f, "LeftUnaryOp({:?},{:?})", t, expr),
            Ast::Assign(ref t, ref expr, ..) => write!(f, "Assign({:?},{:?})", t, expr),
            Ast::Reassign(ref t, ref expr, ..) => write!(f, "Reassign({:?},{:?})", t, expr),
            Ast::Refer(ref t, ..) => write!(f, "Refer({:?})", t),
            Ast::If(ref bool_expr, _, _, is_else, ..) => {
                write!(f, "{}If({:?},_,_)", if is_else { "Else" } else { "" }, bool_expr)
            }
            Ast::While(ref bool_expr, ..) => write!(f, "While({:?},_)", bool_expr),
            Ast::ForIn(None, ref id, ref list, ..) => write!(f, "ForIn({:?}:{:?})", id, list),
            Ast::ForIn(Some(ref idx_id), ref id, ref list, ..) => {
                write!(f, "ForIn({:?},{:?}:{:?})", idx_id, id, list)
            }
            Ast::LoopNav(ref t, ..) => write!(f, "LoopNav({:?})", t),
            Ast::AssignFun(ref t, ref args, ..) => write!(f, "AssignFun({:?},{:?},_)", t, args),
            Ast::Return(ref val, ..) => write!(f, "Return({:?})", val),
            Ast::Call(ref t, ref args, ..) => write!(f, "Call({:?},{:?})", t, args),
            Ast::ReferSeq(ref t, ..) => write!(f, "ReferSeq({:?})", t),
            Ast::Seq(ref exprs, ..) => write!(f, "Seq({:?})", exprs),
            Ast::AssignSeq(ref t, ref seq, ..) => write!(f, "AssignSeq({:?},{:?})", t, seq),
            Ast::ReferSeqIndex(ref t, ref i, ..) => write!(f, "ReferSeqIndex({:?},{:?})", t, i),
            Ast::ReassignSeqIndex(ref t, ref i, ref val, ..) => {
                write!(f, "ReassignSeqIndex({:?},{:?},{:?})", t, i, val)
            }
            Ast::Line(ref scope, ..) => write!(f, "Line({},_)", scope),
            Ast::LinePair(..) => write!(f, "LinePair(_,_)"),
            Ast::Empty(..) => write!(f, "Empty"),
        }
    }
}

impl Ast {
    pub fn bin_op<A: Into<Box<Ast>>>(token: Token, left: A, right: A) -> Ast {
        let left = left.into();
        let right = right.into();
        let src = left.src().up_to_end_of(right.src());

        Ast::BinOp(token, left, right, src)
    }

    pub fn line_pair<A: Into<Box<Ast>>>(before: A, after: A) -> Ast {
        let line = before.into();
        if let Ast::LinePair(..) = *line {
            panic!("LinePair left val must not be a LinePair");
        }
        let after = after.into();
        let src = line.src().up_to_end_of(after.src());
        Ast::LinePair(line, after, src)
    }

    pub fn just_if(expr: Ast, block: Ast, is_else: bool, if_start: SourceRef) -> Ast {
        let src = if_start.up_to_end_of(expr.src());
        Ast::If(expr.into(), block.into(), None, is_else, src)
    }

    pub fn if_else(expr: Ast, block: Ast, else_if: Ast, is_else: bool, if_start: SourceRef) -> Ast {
        let src = if_start.up_to_end_of(expr.src());
        Ast::If(expr.into(), block.into(), Some(else_if.into()), is_else, src)
    }

    pub fn num(n: Int, src: SourceRef) -> Ast {
        Ast::Num(Num(n), src)
    }

    pub fn debug_string(&self) -> String {
        match *self {
            ref pair @ Ast::LinePair(..) => {
                let mut next = pair;
                let mut out = String::new();
                while let Ast::LinePair(ref l1, ref l2, ..) = *next {
                    out = out + &l1.debug_string() + "\n";
                    next = l2;
                }
                out + &next.debug_string()
            }
            Ast::Line(scope, ref expr, src, ..) => {
                format!("{:3}:-{}{}> {}", (src.0).0, scope, "-".repeat(scope), expr.debug_string())
            }
            Ast::If(ref expr, ref block, None, is_else, ..) => format!(
                "{}({})\n{}",
                if is_else { "Else" } else { "If" },
                expr.debug_string(),
                block.debug_string()
            ),
            Ast::If(ref expr, ref block, Some(ref elif), /* is else */ true, src) => format!(
                "ElseIf({})\n{}\n{}",
                expr.debug_string(),
                block.debug_string(),
                elif.debug_string().replacen(
                    "\n",
                    &format!(" (attached to line {})\n", (src.0).0),
                    1
                )
            ),
            Ast::If(ref expr, ref block, Some(ref elif), /* is else */ false, src) => format!(
                "If({})\n{}\n{}",
                expr.debug_string(),
                block.debug_string(),
                elif.debug_string().replacen(
                    "\n",
                    &format!(" (attached to line {})\n", (src.0).0),
                    1
                )
            ),
            Ast::While(ref expr, ref block, ..) => {
                format!("While({})\n{}", expr.debug_string(), block.debug_string())
            }
            ref f @ Ast::ForIn(..) => {
                let mut dbg = format!("{:?}", f);
                if let Ast::ForIn(_, _, _, ref block, ..) = *f {
                    dbg = dbg + "\n" + &block.debug_string();
                }
                dbg
            }
            Ast::AssignFun(ref id, ref args, ref block, ..) => {
                format!("AssignFun({:?}, {:?})\n{}", id, args, block.debug_string())
            }
            ref x => format!("{:?}", x),
        }
    }

    fn is_else_line(&self) -> bool {
        if let Ast::Line(_, ref ast, ..) = *self {
            if let Ast::If(_, _, _, is_else, ..) = **ast {
                return is_else;
            }
        }
        false
    }

    fn line_scope(&self) -> Option<usize> {
        if let Ast::Line(scope, ..) = *self {
            Some(scope)
        } else {
            None
        }
    }

    pub fn src(&self) -> SourceRef {
        use crate::Ast::*;
        match *self {
            Num(.., src)
            | BinOp(.., src)
            | LeftUnaryOp(.., src)
            | Assign(.., src)
            | Reassign(.., src)
            | Refer(.., src)
            | If(.., src)
            | While(.., src)
            | ForIn(.., src)
            | LoopNav(.., src)
            | AssignFun(.., src)
            | Return(.., src)
            | Call(.., src)
            | Seq(.., src)
            | AssignSeq(.., src)
            | ReferSeq(.., src)
            | ReferSeqIndex(.., src)
            | ReassignSeqIndex(.., src)
            | Line(.., src)
            | LinePair(.., src)
            | Empty(.., src) => src,
        }
    }

    /// Returns if Num or `or`/`and`/`not` expression of Nums
    fn is_pure_numeric_expr(&self) -> bool {
        match *self {
            Ast::Num(..) => true,
            Ast::BinOp(And, ref left, ref right, ..) | Ast::BinOp(Or, ref left, ref right, ..) => {
                left.is_pure_numeric_expr() && right.is_pure_numeric_expr()
            }
            Ast::LeftUnaryOp(Not, ref ast, ..) => ast.is_pure_numeric_expr(),
            _ => false,
        }
    }

    fn extract_num(&self) -> Option<i32> {
        match *self {
            Ast::Num(Num(n), ..) => Some(n),
            _ => None,
        }
    }

    fn extract_ref(&self) -> Option<Atom> {
        match *self {
            Ast::Refer(Id(ref id), ..) => Some(id.clone()),
            _ => None,
        }
    }

    fn extract_ref_is_num(&self) -> Option<(Atom, i32)> {
        if let Ast::BinOp(Is, ref left, ref right, ..) = *self {
            if let (Some(id), Some(n)) = (left.extract_ref(), right.extract_num()) {
                return Some((id, n));
            }
        }
        None
    }

    fn extract_fun_call(&self) -> Option<(Atom, &[Ast])> {
        match *self {
            Ast::Call(Id(ref id), ref args, ..) => Some((id.clone(), args)),
            _ => None,
        }
    }

    fn extract_fun_call_is_num(&self) -> Option<((Atom, &[Ast]), i32)> {
        if let Ast::BinOp(Is, ref left, ref right, ..) = *self {
            if let (Some(fun), Some(n)) = (left.extract_fun_call(), right.extract_num()) {
                return Some((fun, n));
            }
        }
        None
    }

    fn validate_bin_op(&self) -> Res<()> {
        match *self {
            Ast::BinOp(And, ref left, ref right, src)
            | Ast::BinOp(Or, ref left, ref right, src) => {
                let lhs_numeric = left.is_pure_numeric_expr();
                let rhs_numeric = right.is_pure_numeric_expr();
                if lhs_numeric && !rhs_numeric || !lhs_numeric && rhs_numeric {
                    let and_or = if let Ast::BinOp(Or, ..) = *self { "or" } else { "and" };

                    let mut msg = format!(
                        "`{lhs} {op} {rhs}` misuse, cannot mix number literals & expressions.",
                        lhs = if lhs_numeric { "NUM" } else { "EXPR" },
                        op = and_or,
                        rhs = if rhs_numeric { "NUM" } else { "EXPR" },
                    );

                    // try to help with common issues
                    if rhs_numeric {
                        if let (Some((id, n)), Some(rhs)) =
                            (left.extract_ref_is_num(), right.extract_num())
                        {
                            // `foo is 1 or 2`
                            msg.push_str(&format!(
                                "\n\nDid you mean `{id} is {n} {op} {id} is {rhs}`?",
                                id = id,
                                n = n,
                                op = and_or,
                                rhs = rhs,
                            ));
                        } else if let (Some(((id, args), n)), Some(rhs)) =
                            (left.extract_fun_call_is_num(), right.extract_num())
                        {
                            // `foo() is 1 or 2`
                            let mut fun_var_name = &id[..id.find('(').unwrap()];
                            if fun_var_name.starts_with("robo_") && fun_var_name != "robo_" {
                                fun_var_name = &fun_var_name["robo_".len()..];
                            }
                            let fun_call = if args.is_empty() { &*id } else { "..." };
                            msg.push_str(&format!(
                                "\n\nTry using a variable `var {id} = {call}`\nthen `{id} is {n} {op} {id} is {rhs}`",
                                id = fun_var_name,
                                call = fun_call,
                                n = n,
                                op = and_or,
                                rhs = rhs,
                            ));
                        }
                    }

                    return Err(BadderError::at(src).describe(Stage::Parser, msg));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

#[inline]
fn id_to_seq_id(id: &Token) -> Token {
    match *id {
        Id(ref id) => Id((id.to_string() + "[]").into()),
        _ => panic!("id_to_seq_id() called on non-Id token"),
    }
}

/// A token that's allowed that wouldn't normaly be
#[derive(Debug, Clone)]
struct TokenGuest {
    token: Token,
    scope: Range<usize>,
}

impl From<(Token, Range<usize>)> for TokenGuest {
    #[inline]
    fn from((token, scope): (Token, Range<usize>)) -> Self {
        Self {
            token,
            scope,
        }
    }
}
impl From<(Token, usize)> for TokenGuest {
    #[inline]
    #[allow(clippy::range_plus_one)]
    fn from((token, scope): (Token, usize)) -> Self {
        Self {
            token,
            scope: scope..scope + 1,
        }
    }
}
impl From<(Token, RangeFrom<usize>)> for TokenGuest {
    #[inline]
    fn from((token, RangeFrom { start }): (Token, RangeFrom<usize>)) -> Self {
        Self {
            token,
            scope: start..usize::max_value(),
        }
    }
}

trait AllowTokenScope {
    fn allow(&self, token: &Token, scope: usize) -> bool;
}

impl AllowTokenScope for TokenGuest {
    fn allow(&self, token: &Token, scope: usize) -> bool {
        self.scope.contains(&scope) && &self.token == token
    }
}
impl<T> AllowTokenScope for T where T: std::ops::Deref<Target = [TokenGuest]> {
    #[inline]
    fn allow(&self, token: &Token, scope: usize) -> bool {
        self.deref().iter().any(|ta| ta.allow(token, scope))
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    previous_token: Option<Token>,
    current_src_ref: SourceRef,
    /// stack of 'next lines' that we want to process later with earlier lines at the top
    unused_lines: Vec<Ast>,
}

enum ListParseEdgeCase {
    DotCall(Ast),
}

impl<'a> Parser<'a> {
    pub fn parse_str(code: &'a str) -> Res<Ast> {
        let mut lexer = Lexer::new(code);
        let (first, src_ref) = lexer.next_token()?;
        Parser {
            lexer,
            current_token: first,
            previous_token: None,
            current_src_ref: src_ref,
            unused_lines: Vec::new(),
        }
        .parse()
    }

    /// Collects all assign ids. Fails only on lexer errors.
    pub fn collect_assign_ids(code: &'a str) -> Res<FxHashSet<AssignId>> {
        let mut lexer = Lexer::new(code);
        let mut ids = FxHashSet::default();
        let mut last_token = None;
        loop {
            match (last_token, lexer.next_token()?) {
                (Some(kind), (Id(id), ..)) => {
                    ids.insert(AssignId { kind, id });
                }
                (_, (Eof, ..)) => break,
                (_, (token, ..)) => last_token = AssignIdKind::try_from(&token),
            }
        }
        Ok(ids)
    }

    fn next_token(&mut self) -> Res<&Token> {
        let (mut token, src_ref) = self.lexer.next_token()?;
        trace!("{:?} @{:?}", token, src_ref);

        ::std::mem::swap(&mut self.current_token, &mut token);
        self.current_src_ref = src_ref;

        self.previous_token = Some(token);
        Ok(&self.current_token)
    }

    fn parse_fail_help(&self) -> Option<Cow<'static, str>> {
        let prev_help: Option<Cow<'static, str>> = match (&self.previous_token, &self.current_token)
        {
            (&Some(Pls), &Pls) => Some("`++` is not an operator, did you mean `+= 1`?".into()),
            (&Some(Is), &Is) => Some("try using just `is`, without stammering.".into()),
            (&Some(Is), token) if token.is_binary_op() => {
                Some(format!("try using just `is` or `{:?}`.", token).into())
            }
            _ => None,
        };

        if prev_help.is_some() {
            return prev_help;
        }

        // next token logic
        let mut lex = self.lexer.clone();
        if let Ok((next_token, ..)) = lex.next_token() {
            match self.current_token {
                Ass => match next_token {
                    Gt => Some("`=>` is not an operator, did you mean `>=`?".into()),
                    Lt => Some("`=<` is not an operator, did you mean `<=`?".into()),
                    Ass => Some("`==` is not an operator, did you mean `is`?".into()),
                    _ => match self.previous_token {
                        Some(Id(_)) | Some(Num(_)) => {
                            Some("cannot assign here, did you mean `is`?".into())
                        }
                        _ => None,
                    },
                },
                Not => match next_token {
                    Gt => Some("`not >` is invalid, did you mean `<=`?".into()),
                    GtEq => Some("`not >=` is invalid, did you mean `<`?".into()),
                    Lt => Some("`not <` is invalid, did you mean `>=`?".into()),
                    LtEq => Some("`not <=` is invalid, did you mean `>`?".into()),
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        }
    }

    fn consume_any(&mut self, types: &[Token]) -> Res<Token> {
        debug_assert!(self.unused_lines.is_empty());

        if types.iter().any(|t| t.matches(&self.current_token)) {
            // eprintln!("{:?}: {:?}", self.current_token, backtrace::Backtrace::new());
            let res = Ok(self.current_token.clone());
            self.next_token()?;
            res
        } else {
            // eprintln!("Err {:?}: {:?}", self.current_token, backtrace::Backtrace::new());
            let help_message = self
                .parse_fail_help()
                .map(|msg| Cow::Owned(format!(", {}", msg)))
                .unwrap_or_else(|| {
                    let maybe_expected = types
                        .iter()
                        .filter_map(|t| match t {
                            &Num(_) => Some(Cow::Borrowed("0-9")),
                            &Eol | &Eof => None,
                            token => Some(format!("{:?}", token).into()),
                        })
                        .collect::<Vec<_>>();

                    match (!maybe_expected.is_empty(), types.contains(&Eol)) {
                        (true, true) => format!(
                            ", something like `{}` or the end of the line was expected",
                            maybe_expected.join(",")
                        )
                        .into(),
                        (false, _) => "".into(),
                        (true, false) => {
                            format!(", something like `{}` was expected", maybe_expected.join(","))
                                .into()
                        }
                    }
                });

            Err(BadderError::at(self.current_src_ref).describe(
                Stage::Parser,
                format!(
                    "Unexpected {}{}{}",
                    self.current_token.long_debug(),
                    self.previous_token
                        .as_ref()
                        .map(|t| Cow::Owned(format!(" after `{:?}`", t)))
                        .unwrap_or_else(|| "".into()),
                    help_message,
                ),
            ))
        }
    }

    fn consume_any_maybe(&mut self, types: &[Token]) -> Res<Option<Token>> {
        debug_assert!(self.unused_lines.is_empty());

        if types.iter().any(|t| t.matches(&self.current_token)) {
            let res = Ok(Some(self.current_token.clone()));
            self.next_token()?;
            return res;
        }
        Ok(None)
    }

    fn consume(&mut self, t: Token) -> Res<Token> {
        self.consume_any(&[t])
    }

    fn consume_maybe(&mut self, t: Token) -> Res<Option<Token>> {
        self.consume_any_maybe(&[t])
    }

    fn fun_call(&mut self, left: Option<Ast>, id: Token) -> Res<Ast> {
        // function call
        let src = left.as_ref().map(Ast::src).unwrap_or(self.current_src_ref);
        self.consume(OpnBrace)?;
        let mut args: Vec<_> = left.into_iter().collect();
        let mut signature = String::new();
        match id {
            Id(name) => signature = signature + &name + "(",
            _ => panic!("fun_call passed in non-Id id"),
        }
        while self.current_token != ClsBrace {
            match self.listref_no_edge_cases()? {
                Ok(Some(list)) => args.push(list),
                Ok(None) => args.push(self.expr()?),
                Err(ListParseEdgeCase::DotCall(..)) => {
                    panic!("Internal error: Unexpected DotCall edge case");
                }
            }

            if self.consume_maybe(Comma)?.is_none() {
                break;
            }
        }
        let src = src.up_to_end_of(self.current_src_ref);
        self.consume(ClsBrace)?;
        for arg in &args {
            match *arg {
                Ast::Seq(..) | Ast::ReferSeq(..) => signature += "s",
                _ => signature += "v",
            }
        }
        signature += ")";
        Ok(Ast::Call(Id(signature.into()), args, src))
    }

    fn num(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        Ok({
            if self.current_token == OpnBrace {
                // TODO handle list literals
                self.braced()?
            } else if let Some(token) = self.consume_maybe(Num(0))? {
                Ast::Num(token, src)
            } else {
                match self.listref()? {
                    // dot calls handled later
                    Err(ListParseEdgeCase::DotCall(list)) | Ok(Some(list)) => list,
                    Ok(None) => {
                        let id = self.consume_any(&[Id("identifier".into()), Num(0)])?;
                        if self.current_token == OpnBrace {
                            // function call
                            match self.fun_call(None, id)? {
                                Ast::Call(token, ast, s) => {
                                    Ast::Call(token, ast, src.up_to_end_of(s))
                                }
                                _ => unreachable!(),
                            }
                        } else if self.consume_maybe(OpnSqr)?.is_some() {
                            // refer to seq index
                            let index_expr = self.expr()?;
                            let src = src.up_to_end_of(self.current_src_ref);
                            self.consume(ClsSqr)?;
                            Ast::ReferSeqIndex(id_to_seq_id(&id), index_expr.into(), src)
                        } else {
                            // refer to an id
                            Ast::Refer(id, src)
                        }
                    }
                }
            }
        })
    }

    fn dotcall(&mut self) -> Res<Ast> {
        let mut out = self.num()?;
        while self.consume_maybe(Dot)?.is_some() {
            let fun_id = self.consume(Id("identifier".into()))?;
            out = self.fun_call(Some(out), fun_id)?;
        }
        Ok(out)
    }

    fn signed(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        if self.current_token == Sub {
            self.next_token()?;
            let negated = self.dotcall()?;
            let src = src.up_to_end_of(self.current_src_ref);
            return Ok(if let Ast::Num(Num(n), _) = negated {
                // Simplify AST for negated number literals
                Ast::Num(Num(-n), src)
            } else {
                Ast::LeftUnaryOp(Sub, negated.into(), src)
            });
        }
        self.dotcall()
    }

    fn multied(&mut self) -> Res<Ast> {
        let mut out = self.signed()?;
        while let Some(token) = self.consume_any_maybe(&[Mul, Mod, Div])? {
            out = Ast::bin_op(token, out, self.signed()?);
        }
        Ok(out)
    }

    fn added(&mut self) -> Res<Ast> {
        let mut out = self.multied()?;
        while let Some(token) = self.consume_any_maybe(&[Pls, Sub])? {
            if token == Pls {
                out = Ast::bin_op(Pls, out, self.multied()?);
            } else {
                out = Ast::bin_op(Sub, out, self.multied()?);
            }
        }
        Ok(out)
    }

    fn compared(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        let mut out = self.added()?;
        if self.consume_maybe(Is)?.is_some() {
            if self.consume_maybe(Not)?.is_some() {
                let is = Ast::bin_op(Is, out, self.added()?);
                out = Ast::LeftUnaryOp(Not, is.into(), src.up_to_end_of(self.current_src_ref));
            } else {
                out = Ast::bin_op(Is, out, self.added()?);
            }
        } else if let Some(token) = self.consume_any_maybe(&[Gt, Lt, GtEq, LtEq])? {
            out = Ast::bin_op(token, out, self.added()?);
        }
        Ok(out)
    }

    fn inversed(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        if self.consume_maybe(Not)?.is_some() {
            Ok(Ast::LeftUnaryOp(
                Not,
                self.compared()?.into(),
                src.up_to_end_of(self.current_src_ref),
            ))
        } else {
            self.compared()
        }
    }

    fn anded(&mut self) -> Res<Ast> {
        let mut out = self.inversed()?;
        while self.consume_maybe(And)?.is_some() {
            out = Ast::bin_op(And, out, self.inversed()?);
            out.validate_bin_op()?;
        }
        Ok(out)
    }

    fn ored(&mut self) -> Res<Ast> {
        let mut out = self.anded()?;
        while self.consume_maybe(Or)?.is_some() {
            out = Ast::bin_op(Or, out, self.anded()?);
            out.validate_bin_op()?;
        }
        Ok(out)
    }

    fn braced(&mut self) -> Res<Ast> {
        if self.consume_maybe(OpnBrace)?.is_some() {
            let out = self.expr()?;
            self.consume(ClsBrace)?;
            return Ok(out);
        }
        self.expr()
    }

    #[inline]
    fn expr(&mut self) -> Res<Ast> {
        self.ored()
    }

    // If expr
    //     line+
    fn line_if_else(&mut self, scope: usize, mut guests: Rc<Vec<TokenGuest>>) -> Res<Ast> {
        let src = self.current_src_ref;
        let (expr, is_else) = {
            if self.consume_maybe(If)?.is_some() {
                (self.expr()?, false)
            } else {
                let src = src.up_to_end_of(self.current_src_ref);
                self.consume(Else)?;
                (
                    self.consume_maybe(If)?
                        .map(|_| self.expr())
                        .unwrap_or_else(|| Ok(Ast::Num(Num(1), src)))?,
                    true,
                )
            }
        };
        self.consume(Eol)?;
        if !guests.allow(&Else, scope) {
            let mut new_guests: Vec<TokenGuest> = guests.as_ref().clone();
            new_guests.push((Else, scope).into());
            guests = new_guests.into();
        }
        let block = self.lines_while_allowing(
            scope + 1,
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            &guests,
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_next_line())
                .describe(Stage::Parser, "Expected line after `if,else` with exactly +1 indent"));
        }
        // else will be in unused_lines as they would mark the end of an if block
        if let Some(line) = self.unused_lines.pop() {
            // must match scope, otherwise could be a valid else for a parent if
            if line.is_else_line() && line.line_scope() == Some(scope) {
                return Ok(Ast::if_else(expr, block.unwrap(), line, is_else, src));
            }
            self.unused_lines.push(line);
        }
        Ok(Ast::just_if(expr, block.unwrap(), is_else, src))
    }

    // loop
    //     line+
    fn line_loop(&mut self, scope: usize, guests: Rc<Vec<TokenGuest>>) -> Res<Ast> {
        let src = self.current_src_ref;
        let (while_expr, for_stuff) = {
            if self.consume_maybe(While)?.is_some() {
                (self.expr()?, None)
            } else if self.consume_maybe(For)?.is_some() {
                let mut idx_id = Some(self.consume(Id("identifier".into()))?);
                let item_id = match self.consume_maybe(Comma)? {
                    Some(_) => self.consume(Id("identifier".into()))?,
                    _ => idx_id.take().unwrap(),
                };

                self.consume(In)?;
                let list = self.list()?;
                (Ast::Empty(src.up_to_end_of(self.current_src_ref)), Some((idx_id, item_id, list)))
            } else {
                self.consume(Loop)?;
                (Ast::num(1, src.up_to_end_of(self.current_src_ref)), None)
            }
        };
        self.consume(Eol)?;
        let loop_allow = {
            if guests.allow(&Break, scope) && guests.allow(&Continue, scope) {
                guests
            } else {
                let mut extended = vec![(Break, scope + 1..).into(), (Continue, scope + 1..).into()];
                extended.extend_from_slice(guests.as_slice());
                extended.into()
            }
        };
        let block = self.lines_while_allowing(
            scope + 1,
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            &loop_allow,
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_next_line()).describe(
                Stage::Parser,
                "Expected line after `loop,while,for` with exactly +1 indent",
            ));
        }
        Ok(if let Some((idx_id, item_id, list)) = for_stuff {
            let src = src.up_to_end_of(list.src());
            Ast::ForIn(idx_id, item_id, list.into(), block.unwrap().into(), src)
        } else {
            let src = src.up_to_end_of(while_expr.src());
            Ast::While(while_expr.into(), block.unwrap().into(), src)
        })
    }

    // fun id()
    //     line+
    fn line_fun(&mut self, scope: usize) -> Res<Ast> {
        let src = self.current_src_ref;
        self.consume(Fun)?;
        let id_name: Atom = "identifier".into();
        let id = self.consume(Id(id_name.clone()))?;
        self.consume(OpnBrace)?;
        let mut signature = String::new();
        match id {
            Id(fun_name) => signature = signature + &fun_name + "(",
            _ => unreachable!(),
        };
        let mut arg_ids = vec![];
        while let Some(mut arg) = self.consume_maybe(Id(id_name.clone()))? {
            if self.consume_maybe(Square)?.is_some() {
                arg = id_to_seq_id(&arg);
                signature += "s"
            } else {
                signature += "v";
            }
            arg_ids.push(arg);
            if self.consume_maybe(Comma)?.is_none() {
                break;
            }
        }
        let src = src.up_to_end_of(self.current_src_ref);
        self.consume(ClsBrace)?;
        self.consume(Eol)?;
        signature += ")";
        let block = self.lines_while_allowing(
            scope + 1,
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            &Rc::new(vec![(Return, scope + 1..).into()]),
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_next_line()).describe(
                Stage::Parser,
                "Expected line after `fun` declaration with exactly +1 indent",
            ));
        }
        Ok(Ast::AssignFun(Id(signature.into()), arg_ids, block.unwrap().into(), src))
    }

    /// return optional list match
    fn _listref(&mut self, edge_cases: bool) -> Res<Result<Option<Ast>, ListParseEdgeCase>> {
        if self.consume_maybe(OpnBrace)?.is_some() {
            let list = self.list()?;
            self.consume(ClsBrace)?;
            return Ok(Ok(Some(list)));
        }
        let src = self.current_src_ref;
        if let Id(..) = self.current_token {
            if self.lexer.peek()? == Square && (edge_cases || self.lexer.peekn(2)? != Dot) {
                let id = self.consume(Id("identifier".into()))?;
                self.consume(Square)?;
                let refer =
                    Ast::ReferSeq(id_to_seq_id(&id), src.up_to_end_of(self.current_src_ref));
                return if self.current_token == Dot {
                    Ok(Err(ListParseEdgeCase::DotCall(refer)))
                } else {
                    Ok(Ok(Some(refer)))
                };
            }
        }
        Ok(Ok(None))
    }

    #[inline]
    fn listref(&mut self) -> Res<Result<Option<Ast>, ListParseEdgeCase>> {
        self._listref(true)
    }

    #[inline]
    fn listref_no_edge_cases(&mut self) -> Res<Result<Option<Ast>, ListParseEdgeCase>> {
        self._listref(false)
    }

    fn list(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        match self.listref()? {
            Ok(Some(list)) => return Ok(list),
            Err(ListParseEdgeCase::DotCall(..)) => {
                // functions cannot (yet) return seqs so this can't work
                return Err(BadderError::at(self.current_src_ref).describe(
                    Stage::Parser,
                    format!(
                        "Expected sequence reference/literal got `{}`",
                        self.current_token.long_debug()
                    ),
                ));
            }
            Ok(None) => (),
        }
        let mut exprs = vec![self.expr()?];
        while self.consume_maybe(Comma)?.is_some() {
            exprs.push(self.expr()?);
        }
        Ok(Ast::Seq(exprs, src.up_to(self.current_src_ref)))
    }

    // seq id[] = expr,expr,
    fn line_seq(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        self.consume(Seq)?;
        let seq_id = id_to_seq_id(&self.consume(Id("identifier".into()))?);
        self.consume(Square)?;
        Ok(if self.consume_maybe(Ass)?.is_some() {
            Ast::AssignSeq(seq_id, self.list()?.into(), src.up_to(self.current_src_ref))
        } else {
            let src = src.up_to(self.current_src_ref);
            Ast::AssignSeq(seq_id, Ast::Seq(vec![], src).into(), src)
        })
    }

    fn line_expr(&mut self, scope: usize, guests: Rc<Vec<TokenGuest>>) -> Res<Ast> {
        let src = self.current_src_ref;

        if guests.allow(&self.current_token, scope) {
            if let Some(token) = self.consume_any_maybe(&[Break, Continue])? {
                return Ok(Ast::LoopNav(token, src));
            }
            if self.consume_maybe(Return)?.is_some() {
                let expr = {
                    if self.consume_maybe(Eol)?.is_none() {
                        let return_expr = self.expr()?;
                        self.consume(Eol)?;
                        return_expr
                    } else {
                        Ast::num(0, self.current_src_ref)
                    }
                };
                let src = src.up_to_end_of(expr.src());
                return Ok(Ast::Return(expr.into(), src));
            }
        }

        // var id = expr
        if self.consume_maybe(Var)?.is_some() {
            let id = self.consume(Id("identifier".into()))?;
            if self.consume_maybe(Ass)?.is_none() {
                let src = src.up_to(self.current_src_ref);
                self.consume_any(&[Ass, Eol, Eof])?;
                return Ok(Ast::Assign(id, Ast::Num(Num(0), src).into(), src));
            }
            let expr = self.expr()?;
            let expr_src = expr.src();
            self.consume_any(&[Eol, Eof])?;
            return Ok(Ast::Assign(id, expr.into(), src.up_to_end_of(expr_src)));
        }
        // id = expr
        if let Id(_) = self.current_token {
            let peek = self.lexer.peek()?;
            if peek == Ass {
                let id = self.consume(Id("identifier".into()))?;
                self.consume(Ass)?;
                let expr = self.expr()?;
                let src = src.up_to_end_of(expr.src());
                self.consume_any(&[Eol, Eof])?;
                return Ok(Ast::Reassign(id, expr.into(), src));
            }
            if let OpAss(op) = peek {
                let id_src = self.current_src_ref;
                let id = self.consume(Id("identifier".into()))?;
                let refer = Ast::Refer(id.clone(), id_src);
                self.next_token()?;
                let expr = self.expr()?;
                let src = src.up_to_end_of(expr.src());
                self.consume_any(&[Eol, Eof])?;
                // v *= expr  ->  v = v * expr
                return Ok(Ast::Reassign(id, Ast::bin_op(*op, refer, expr).into(), src));
            }
            if peek == OpnSqr {
                // ReferSeqIndex|ReassignSeqIndex
                let mut out = self.num()?;
                if self.consume_maybe(Ass)?.is_some() {
                    let expr = self.expr()?;
                    out = match out {
                        Ast::ReferSeqIndex(id, idx, src) => {
                            let src = src.up_to_end_of(expr.src());
                            Ast::ReassignSeqIndex(id, idx, expr.into(), src)
                        }
                        _ => {
                            return Err(BadderError::at(src.up_to_end_of(self.current_src_ref))
                                .describe(
                                    Stage::Parser,
                                    format!("expecting seq index ref id[expr], got {:?}", out),
                                ));
                        }
                    }
                }
                return Ok(out);
            }
        }
        // If expr
        //     line+
        if self.current_token == If || guests.allow(&Else, scope) && self.current_token == Else {
            return self.line_if_else(scope, guests);
        }
        // loop
        //     line+
        if [While, Loop, For].contains(&self.current_token) {
            return self.line_loop(scope, guests);
        }
        // fun id()
        //     line+
        if self.current_token == Fun {
            return self.line_fun(scope);
        }
        // seq id[] = expr,expr,
        if self.current_token == Seq {
            return self.line_seq();
        }
        // expr
        if self.current_token != Eof {
            let out = self.expr()?;
            self.consume_any(&[Eol, Eof])?;
            return Ok(out);
        }

        self.consume(Eof)?;
        Ok(Ast::Empty(src))
    }

    fn indented_line(&mut self, allow: Rc<Vec<TokenGuest>>) -> Res<Ast> {
        if let Some(line) = self.unused_lines.pop() {
            return Ok(line);
        }

        let mut src = self.current_src_ref;
        let mut scope = 0;
        while let Some(token) = self.consume_any_maybe(&[Eol, Indent(0)])? {
            match token {
                Indent(x) => scope = x,
                Eol => {
                    scope = 0;
                    src = self.current_src_ref;
                } // reset scope, and skip empty lines
                _ => unreachable!(),
            };
        }
        Ok(match self.line_expr(scope, allow)? {
            Ast::Empty(s) => Ast::Empty(s),
            ast => {
                let ast_src = ast.src();
                Ast::Line(scope, ast.into(), src.up_to_end_of(ast_src))
            }
        })
    }

    /// Checks a line as the expected scope
    #[inline]
    fn check_line_has_scope(ast: &Ast, expected_scope: usize) -> Res<()> {
        if let Ast::Line(scope, _, src) = *ast {
            if scope != expected_scope {
                let indent_src_ref = src.with_char_end(((src.0).0, scope * 4 + 1));
                let hint = if expected_scope > scope {
                    format!("try adding {}", expected_scope - scope)
                } else {
                    format!("try removing {}", scope - expected_scope)
                };
                return Err(BadderError::at(indent_src_ref).describe(
                    Stage::Parser,
                    format!(
                        "Incorrect indentation, expected {} indent(s) {}",
                        expected_scope, hint,
                    ),
                ));
            }
        }
        Ok(())
    }

    /// Checks for correct indent from one line to the next
    /// indentation should only increase by exactly 1 after an if,loop,for,while,fun
    #[inline]
    fn check_scope_change(line1: &Ast, line2: &Ast) -> Res<()> {
        if let (&Ast::Line(prev_scope, ref prev_expr, ..), &Ast::Line(scope, _, src)) =
            (line1, line2)
        {
            if scope > prev_scope {
                match **prev_expr {
                    Ast::If(..) | Ast::While(..) | Ast::ForIn(..) | Ast::AssignFun(..)
                        if scope == prev_scope + 1 => {}

                    Ast::If(..) | Ast::While(..) | Ast::ForIn(..) | Ast::AssignFun(..) => {
                        let indent_src_ref = src.with_char_end(((src.0).0, scope * 4 + 1));
                        return Err(BadderError::at(indent_src_ref)
                            .describe(
                                Stage::Parser,
                                "Incorrect indentation, exactly +1 indent must used after a line starting with `if,loop,for,while,fun`"
                            ));
                    }

                    _ => {
                        let indent_src_ref = src.with_char_end(((src.0).0, scope * 4 + 1));
                        return Err(BadderError::at(indent_src_ref)
                            .describe(
                                Stage::Parser,
                                "Incorrect indentation, +1 indent can only occur after a line starting with `if,loop,for,while,fun`"
                            ));
                    }
                }
            }
        }
        Ok(())
    }

    fn lines_while_allowing<F>(
        &mut self,
        first_line_scope: usize,
        predicate: F,
        allow: &Rc<Vec<TokenGuest>>,
    ) -> Res<Option<Ast>>
    where
        F: Fn(&Ast) -> bool,
    {
        let mut all = vec![];
        let mut line = self.indented_line(Rc::clone(allow))?;

        while match line {
            Ast::Empty(..) => false,
            ref l => predicate(l),
        } {
            if all.is_empty() {
                Self::check_line_has_scope(&line, first_line_scope)?;
            }
            if let (line, Some(prev_line)) = (&line, all.last()) {
                Self::check_scope_change(prev_line, line)?;
            }

            all.push(line);
            line = self.indented_line(Rc::clone(allow))?;
        }
        self.unused_lines.push(line);

        let mut pairs: Option<Ast> = None;
        let mut idx = all.len();

        while idx != 0 {
            pairs = match pairs {
                None => Some(all.remove(idx - 1)),
                Some(x) => Some(Ast::line_pair(all.remove(idx - 1), x)),
            };
            idx -= 1;
        }
        Ok(pairs)
    }

    fn lines_while<F>(&mut self, predicate: F) -> Res<Option<Ast>>
    where
        F: Fn(&Ast) -> bool,
    {
        self.lines_while_allowing(0, predicate, &Rc::new(vec![]))
    }

    pub fn parse(&mut self) -> Res<Ast> {
        let lines = self.lines_while(|_| true)?;
        Ok(lines.unwrap_or_else(|| Ast::Empty(self.current_src_ref)))
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignId {
    pub id: Atom,
    pub kind: AssignIdKind,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignIdKind {
    Var,
    Fun,
    Seq,
}
impl AssignIdKind {
    fn try_from(token: &Token) -> Option<Self> {
        Some(match token {
            Token::Var => AssignIdKind::Var,
            Token::Seq => AssignIdKind::Seq,
            Token::Fun => AssignIdKind::Fun,
            _ => return None,
        })
    }
}

#[cfg(test)]
#[allow(clippy::cognitive_complexity)]
mod parser_test {
    use super::*;

    macro_rules! src_eq {
        ($left:expr, $right:expr) => {{
            let src = $left.src();
            if src != $right {
                println!(
                    "Unexpected src for `{}`, `{:?}` != `{:?}`",
                    $left.debug_string(),
                    src,
                    $right
                );
                false
            } else {
                true
            }
        }};
    }

    macro_rules! assert_src_eq {
        ($left:expr, $right:expr) => {{
            if src_eq!($left, $right) {
                assert_eq!($left.src(), $right);
            }
        }};
    }

    /// Bit dodgy as you need to use the `$ast` ident in the pattern
    macro_rules! expect_ast {
        ($ast:ident = $pat:pat) => {
            match $ast {
                $pat => $ast,
                _ => panic!("Unexpected {}", $ast.debug_string()),
            }
        };
        ($ast:ident = $pat:pat,src = $src:expr) => {{
            let src_eq = src_eq!($ast, $src);
            #[allow(clippy::match_ref_pats)]
            let out = match $ast {
                $pat => $ast,
                _ => panic!("Unexpected {}", $ast.debug_string()),
            };
            if !src_eq {
                assert!(false, "Unexpected SourceRef");
            }
            out
        }};
    }

    macro_rules! expect_line_pair {
        ($ast:ident) => {
            match $ast {
                Ast::LinePair(l1, l2, ..) => (*l1, *l2),
                _ => panic!("Unexpected {}", $ast.debug_string()),
            }
        };
    }

    macro_rules! expect_bin_op {
        ($ast:ident,src = $src:expr) => {{
            src_eq!($ast, $src); // just print
            match $ast {
                Ast::BinOp(_, left, right, src) => {
                    assert_eq!(src, $src);
                    (*left, *right)
                }
                _ => panic!("Unexpected {}", $ast.debug_string()),
            }
        }};
    }

    macro_rules! expect_if_ast {
        ($ast:ident,src = $src:expr) => {{
            src_eq!($ast, $src); // just print
            match $ast {
                Ast::If(expr, block, elif, _, src) => {
                    assert_eq!(src, $src);
                    (*expr, block.as_ref().clone(), elif.map(|boxed| *boxed))
                }
                _ => panic!("Unexpected {}", $ast.debug_string()),
            }
        }};
    }

    #[test]
    fn seq_src_ref() {
        let _ = env_logger::try_init();

        let mut ast = Parser::parse_str("seq some_id[] = 1345, 2").unwrap();

        ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((1, 1), (1, 24)));
        ast = *expect_ast!(
            ast = Ast::AssignSeq(_, ast, ..),
            src = SourceRef((1, 1), (1, 24))
        );
        let seq_ast: Vec<Ast> =
            expect_ast!(ast = Ast::Seq(ast, ..), src = SourceRef((1, 17), (1, 24)));

        assert_eq!(seq_ast.len(), 2);
        assert_src_eq!(seq_ast[0], SourceRef((1, 25), (1, 29)));
        assert_src_eq!(seq_ast[1], SourceRef((1, 31), (1, 32)));
    }

    #[test]
    fn assign_var() {
        let _ = env_logger::try_init();

        let ast = Parser::parse_str(&vec!["var abc", "var bcd"].join("\n")).unwrap();

        let (ast, next) = expect_line_pair!(ast);

        // `var abc`
        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((1, 1), (1, 8)));
        let ast = *expect_ast!(ast = Ast::Assign(_, ast, ..), src = SourceRef((1, 1), (1, 8)));
        expect_ast!(ast = Ast::Num(ast, ..), src = SourceRef((1, 1), (1, 8)));

        // `var bcd`
        let ast = *expect_ast!(next = Ast::Line(0, next, ..), src = SourceRef((2, 1), (2, 8)));
        let ast = *expect_ast!(ast = Ast::Assign(_, ast, ..), src = SourceRef((2, 1), (2, 8)));
        expect_ast!(ast = Ast::Num(ast, ..), src = SourceRef((2, 1), (2, 8)));
    }

    #[test]
    fn if_else_src_ref() {
        let _ = env_logger::try_init();

        let ast = Parser::parse_str(
            &vec![
                "var a = 123",
                "if a > 100",
                "    a *= 2",
                "else if a > 50",
                "    a = a / 3",
                "else",
                "    a += 32",
            ]
            .join("\n"),
        )
        .unwrap();

        let (ast, next) = expect_line_pair!(ast);

        // `var a = 123`
        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((1, 1), (1, 12)));
        let ast = *expect_ast!(ast = Ast::Assign(_, ast, ..), src = SourceRef((1, 1), (1, 12)));
        expect_ast!(ast = Ast::Num(ast, ..), src = SourceRef((1, 9), (1, 12)));

        // `if a > 100`
        let ast = *expect_ast!(next = Ast::Line(0, next, ..), src = SourceRef((2, 1), (2, 11)));
        let (expr, block, next) = expect_if_ast!(ast, src = SourceRef((2, 1), (2, 11)));
        let (left, right) = expect_bin_op!(expr, src = SourceRef((2, 4), (2, 11)));
        expect_ast!(left = Ast::Refer(..), src = SourceRef((2, 4), (2, 5)));
        expect_ast!(right = Ast::Num(Num(100), ..), src = SourceRef((2, 8), (2, 11)));

        // `    a *= 2` -> `    a = a * 2`
        let ast = *expect_ast!(block = Ast::Line(1, block, ..), src = SourceRef((3, 1), (3, 11)));
        let ast = *expect_ast!(ast = Ast::Reassign(_, ast, ..), src = SourceRef((3, 5), (3, 11)));
        let (left, right) = expect_bin_op!(ast, src = SourceRef((3, 5), (3, 11)));
        expect_ast!(left = Ast::Refer(..), src = SourceRef((3, 5), (3, 6)));
        expect_ast!(right = Ast::Num(Num(2), ..), src = SourceRef((3, 10), (3, 11)));

        // `else if a > 50`
        let ast = next.expect("else");
        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((4, 1), (4, 15)));
        let (expr, block, next) = expect_if_ast!(ast, src = SourceRef((4, 1), (4, 15)));
        let (left, right) = expect_bin_op!(expr, src = SourceRef((4, 9), (4, 15)));
        expect_ast!(left = Ast::Refer(..), src = SourceRef((4, 9), (4, 10)));
        expect_ast!(right = Ast::Num(Num(50), ..), src = SourceRef((4, 13), (4, 15)));

        // `    a = a / 3`
        let ast = *expect_ast!(block = Ast::Line(1, block, ..), src = SourceRef((5, 1), (5, 14)));
        let ast = *expect_ast!(ast = Ast::Reassign(_, ast, ..), src = SourceRef((5, 5), (5, 14)));
        let (left, right) = expect_bin_op!(ast, src = SourceRef((5, 9), (5, 14)));
        expect_ast!(left = Ast::Refer(..), src = SourceRef((5, 9), (5, 10)));
        expect_ast!(right = Ast::Num(Num(3), ..), src = SourceRef((5, 13), (5, 14)));

        // `else`
        let ast = next.expect("else");
        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((6, 1), (6, 5)));
        let (_, _, next) = expect_if_ast!(ast, src = SourceRef((6, 1), (6, 5)));
        assert_eq!(next, None);
    }

    #[test]
    fn fun_src_ref() {
        let _ = env_logger::try_init();

        let ast = Parser::parse_str(
            &vec![
                "fun double(x)",
                "    return x * 2 # used return to test it, also this comment",
                "double(2.double())",
            ]
            .join("\n"),
        )
        .unwrap();

        let (ast, next) = expect_line_pair!(ast);

        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((1, 1), (1, 14)));
        let ast = &*expect_ast!(
            ast = Ast::AssignFun(.., ast, _),
            src = SourceRef((1, 1), (1, 14))
        );
        let ast = &**expect_ast!(
            ast = &Ast::Line(1, ref ast, ..),
            src = SourceRef((2, 1), (2, 17))
        );
        expect_ast!(ast = &Ast::Return(ref ast, ..), src = SourceRef((2, 5), (2, 17)));

        let ast = *expect_ast!(next = Ast::Line(0, next, ..), src = SourceRef((3, 1), (3, 19)));
        let ast =
            expect_ast!(ast = Ast::Call(_, ast, ..), src = SourceRef((3, 1), (3, 19))).remove(0);
        let ast =
            expect_ast!(ast = Ast::Call(_, ast, ..), src = SourceRef((3, 8), (3, 18))).remove(0);
        expect_ast!(ast = Ast::Num(Num(2), ..), src = SourceRef((3, 8), (3, 9)));
    }

    #[test]
    fn parse_error() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(
            &vec![
                "fum double(x)", // `fun` misspelled -> id
                "    return x * 2",
            ]
            .join("\n"),
        )
        .expect_err("parse");

        println!("Got error: {:?}", err);

        assert!(err.description.contains("double"));
        assert_eq!(err.src, SourceRef((1, 5), (1, 11)));
    }

    #[test]
    fn collect_assign_ids() {
        #[rustfmt::skip]
        let code = "var foo = 123\n\
                    foo += 234\n\
                    fun do_something()\n    \
                        seq foos[]\n    \
                        something()\n\
                    if foo > 345\n    \
                        var bar = 45\n    \
                        do_something()\n\
                    else\n    \
                        var foo2\n    \
                        if foo < 0\n        \
                            var bar2 = 12";
        let ids = Parser::collect_assign_ids(code).unwrap();
        assert_eq!(
            ids,
            vec![
                AssignId { kind: AssignIdKind::Var, id: Atom::from("foo") },
                AssignId { kind: AssignIdKind::Fun, id: Atom::from("do_something") },
                AssignId { kind: AssignIdKind::Seq, id: Atom::from("foos") },
                AssignId { kind: AssignIdKind::Var, id: Atom::from("bar") },
                AssignId { kind: AssignIdKind::Var, id: Atom::from("foo2") },
                AssignId { kind: AssignIdKind::Var, id: Atom::from("bar2") },
            ]
            .into_iter()
            .collect()
        );
    }
}

#[cfg(test)]
mod helpful_error {
    use super::*;

    #[test]
    fn reversed_greater_or_equal() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(
            &vec![
                "if 12 => 11", // `=>` misspelled
                "    0",
            ]
            .join("\n"),
        )
        .expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 7), (1, 8)));
        assert!(err.description.contains("`>=`"), "error did not suggest `>=`");
    }

    #[test]
    fn reversed_less_than_or_equal() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(
            &vec![
                "if 12 =< 11", // `=>` misspelled
                "    0",
            ]
            .join("\n"),
        )
        .expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 7), (1, 8)));
        assert!(err.description.contains("`<=`"), "error did not suggest `<=`");
    }

    #[test]
    fn var_plus_plus() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["var x", "x++"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((2, 3), (2, 4)));
        assert!(err.description.contains("`+= 1`"), "error did not suggest `+= 1`");
    }

    #[test]
    fn is_is_greater_than() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["12 is > 11"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 7), (1, 8)));
        assert!(err.description.contains("`is` or `>`"), "error did not suggest `is` or `>`");
    }

    #[test]
    fn not_greater_than() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["12 not > 11"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 4), (1, 7)));
        assert!(err.description.contains("`<=`"), "error did not suggest `<=`");
    }

    #[test]
    fn not_less_than_or_equal_to() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["12 not <= 11"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 4), (1, 7)));
        assert!(err.description.contains("`>`"), "error did not suggest `>`");
    }

    #[test]
    fn assignment_instead_of_is() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["if 12 = 11", "    0"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 7), (1, 8)));
        assert!(err.description.contains("`is`"), "error did not suggest `is`");
    }

    #[test]
    fn double_assignment_instead_of_is() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["if 12 == 11", "    0"].join("\n")).expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((1, 7), (1, 8)));
        assert!(err.description.contains("`is`"), "error did not suggest `is`");
    }

    #[test]
    fn variable_is_1_or_2() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(&vec!["var scan = 1", "if scan is 1 or 2", "    0"].join("\n"))
            .expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((2, 4), (2, 18)));
        assert!(err.description.contains("`scan is 1 or scan is 2`"));
    }

    #[test]
    fn argless_fun_call_is_1_or_2() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(
            &vec!["fun do_scan()", "    2", "if do_scan() is 1 or 2", "    0"].join("\n"),
        )
        .expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((3, 4), (3, 23)));
        assert!(err.description.contains("var do_scan = do_scan()"));
        assert!(err.description.contains("do_scan is 1 or do_scan is 2"));
    }

    #[test]
    fn arg_fun_call_is_1_or_2() {
        let _ = env_logger::try_init();

        let err = Parser::parse_str(
            &vec!["fun do_scan(n)", "    n", "if do_scan(2) is 1 or 2", "    0"].join("\n"),
        )
        .expect_err("parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((3, 4), (3, 24)));
        assert!(err.description.contains("var do_scan = "));
        assert!(err.description.contains("do_scan is 1 or do_scan is 2"));
    }
}

// reproductions of encountered issues/bugs
#[cfg(test)]
mod issues {
    use super::*;

    #[test]
    fn else_after_var() {
        let _ = env_logger::try_init();
        Parser::parse_str(
            &vec![
                "if 1",
                "    if 1",
                "        0",
                "    var foo = 123",
                "    else",
                "        fail()",
            ]
            .join("\n"),
        )
        .expect_err("did not fail parse");
    }

    #[test]
    fn return_in_function_body_with_loops() {
        let _ = env_logger::try_init();
        Parser::parse_str(
            &vec![
                "fun some_func()",
                "    for i in 1,2,3",
                "        if i > 4",
                "            return 234",
                "    return 123",
                "some_func()",
            ]
            .join("\n"),
        )
        .expect("parse");
    }

    #[test]
    fn seq_nested_dot_calling() {
        let _ = env_logger::try_init();
        Parser::parse_str(
            &vec![
                "fun plus(a1, a2)",
                "    a1 + a2",
                "seq list[]",
                "list[].add(list[].size().plus(123))",
            ]
            .join("\n"),
        )
        .expect("parse");
    }

    #[test]
    fn over_indenting() {
        let _ = env_logger::try_init();
        let err = Parser::parse_str(&vec!["var x", "    x = 1", "        x += 12"].join("\n"))
            .expect_err("did not fail parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((2, 1), (2, 5)));
        assert!(err.description.contains("indent"), "error did contain 'indent'");
    }

    #[test]
    fn over_indenting_after_if() {
        let _ = env_logger::try_init();
        let err = Parser::parse_str(&vec!["var x", "if x is 0", "            x += 12"].join("\n"))
            .expect_err("did not fail parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((3, 1), (3, 13)));
        assert!(err.description.contains("indent"), "error did contain 'indent'");
    }

    #[test]
    fn boolean_mix_expr_number_literal() {
        let _ = env_logger::try_init();
        let err = Parser::parse_str(
            &vec![
                "var x = 4",
                "if x is 1 or 2", // misuse of boolean operator
                "    x += 1",
                "x",
            ]
            .join("\n"),
        )
        .expect_err("did not fail parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((2, 4), (2, 15)));
        assert!(err.description.contains("or"));

        let err = Parser::parse_str(
            &vec![
                "var x = 4",
                "if 2 or x is 1 and 3", // misuse of boolean operator
                "    x += 1",
                "x",
            ]
            .join("\n"),
        )
        .expect_err("did not fail parse");

        println!("Got error: {:?}", err);

        assert_eq!(err.src, SourceRef((2, 9), (2, 21)));
        assert!(err.description.contains("and"));
    }

    #[test]
    fn size_minus_1() {
        let _ = env_logger::try_init();
        Parser::parse_str(
            &vec![
                "seq path[] = 1, 2, 3",
                "path[].remove(path[].size() - 1)", // misuse of boolean operator
            ]
            .join("\n"),
        )
        .expect("did not parse");
    }
}
