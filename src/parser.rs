use super::Int;
use lexer::*;
use lexer::Token::*;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use string_cache::DefaultAtom as Atom;
use common::*;

/// Abstract syntax tree
#[derive(PartialEq, Clone)]
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ast::Num(ref t, ..) => write!(f, "Num({:?})", t),
            Ast::BinOp(ref t, ref l, ref r, ..) => write!(f, "BinOp({:?},{:?},{:?})", t, l, r),
            Ast::LeftUnaryOp(ref t, ref expr, ..) => write!(f, "LeftUnaryOp({:?},{:?})", t, expr),
            Ast::Assign(ref t, ref expr, ..) => write!(f, "Assign({:?},{:?})", t, expr),
            Ast::Reassign(ref t, ref expr, ..) => write!(f, "Reassign({:?},{:?})", t, expr),
            Ast::Refer(ref t, ..) => write!(f, "Refer({:?})", t),
            Ast::If(ref bool_expr, _, _, is_else, ..) => {
                write!(f, "{}If({:?},_,_)", if is_else { "Else" } else { "" }, bool_expr)
            },
            Ast::While(ref bool_expr, ..) => write!(f, "While({:?},_)", bool_expr),
            Ast::ForIn(None, ref id, ref list, ..) => write!(f, "ForIn({:?}:{:?})", id, list),
            Ast::ForIn(Some(ref idx_id), ref id, ref list, ..) => {
                write!(f, "ForIn({:?},{:?}:{:?})", idx_id, id, list)
            },
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
            },
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
            },
            Ast::Line(scope, ref expr, ..) => {
                format!("-{}{}> {}", scope, "-".repeat(scope), expr.debug_string())
            },
            Ast::If(ref expr, ref block, None, ..) => {
                format!("If({})\n{}", expr.debug_string(), block.debug_string())
            },
            Ast::If(ref expr, ref block, Some(ref elif), ..) => {
                format!("If({})\n{}\nElse{}",
                        expr.debug_string(),
                        block.debug_string(),
                        elif.debug_string())
            },
            Ast::While(ref expr, ref block, ..) => {
                format!("While({})\n{}", expr.debug_string(), block.debug_string())
            },
            ref f @ Ast::ForIn(..) => {
                let mut dbg = format!("{:?}", f);
                if let Ast::ForIn(_,_,_, ref block, ..) = *f {
                    dbg = dbg + "\n" + &block.debug_string();
                }
                dbg
            },
            Ast::AssignFun(ref id, ref args, ref block, ..) => {
                format!("AssignFun({:?}, {:?})\n{}", id, args, block.debug_string())
            },
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

    pub fn src(&self) -> SourceRef {
        use Ast::*;
        match *self {
            Num(.., src) => src,
            BinOp(.., src) => src,
            LeftUnaryOp(.., src) => src,
            Assign(.., src) => src,
            Reassign(.., src) => src,
            Refer(.., src) => src,
            If(.., src) => src,
            While(.., src) => src,
            ForIn(.., src) => src,
            LoopNav(.., src) => src,
            AssignFun(.., src) => src,
            Return(.., src) => src,
            Call(.., src) => src,
            Seq(.., src) => src,
            AssignSeq(.., src) => src,
            ReferSeq(.., src) => src,
            ReferSeqIndex(.., src) => src,
            ReassignSeqIndex(.., src) => src,
            Line(.., src) => src,
            LinePair(.., src) => src,
            Empty(.., src) => src,
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

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    current_src_ref: SourceRef,
    /// stack of 'next lines' that we want to process later with earlier lines at the top
    unused_lines: Vec<Ast>,
}

impl<'a> Parser<'a> {
    pub fn parse_str(code: &'a str) -> Res<Ast> {
        let mut lexer = Lexer::new(code);
        let (first, src_ref) = lexer.next_token()?;
        Parser {
            lexer,
            current_token: first,
            current_src_ref: src_ref,
            unused_lines: Vec::new(),
        }.parse()
    }

    fn next_token(&mut self) -> Res<&Token> {
        let (token, src_ref) = self.lexer.next_token()?;
        trace!("{:?} @{:?}", token, src_ref);
        self.current_token = token;
        self.current_src_ref = src_ref;
        Ok(&self.current_token)
    }

    fn consume_any(&mut self, types: &[Token]) -> Res<Token> {
        assert!(self.unused_lines.is_empty());

        if types.iter().any(|t| t.matches(&self.current_token)) {
            let res = Ok(self.current_token.clone());
            self.next_token()?;
            res
        }
        else {
            let expected = types
                .iter()
                .map(|t| match t {
                    &Num(_) => "0-9".to_string(),
                    token => format!("{:?}", token),
                })
                .collect::<Vec<String>>()
                .join(",");
            Err(BadderError::at(self.current_src_ref)
                .describe(format!("Parser: Expected `{}` got {}",
                                  expected,
                                  self.current_token.long_debug())))
        }
    }

    fn consume_any_maybe(&mut self, types: &[Token]) -> Res<Option<Token>> {
        assert!(self.unused_lines.is_empty());

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
        let src = left.iter().next().map(|a| a.src()).unwrap_or(self.current_src_ref);
        self.consume(OpnBrace)?;
        let mut args = left.map(|a| vec![a]).unwrap_or_else(|| vec![]);
        let mut signature = String::new();
        match id {
            Id(name) => signature = signature + &name + "(",
            _ => panic!("fun_call passed in non-Id id"),
        }
        while self.current_token != ClsBrace {
            match self.listref()? {
                Some(list) => args.push(list),
                _ => args.push(self.expr()?),
            }

            if self.consume_maybe(Comma)?.is_none() {
                break;
            }
        }
        let src = src.up_to_end_of(self.current_src_ref);
        self.consume(ClsBrace)?;
        for arg in &args {
            match *arg {
                Ast::Seq(..) | Ast::ReferSeq(..) => signature += "l",
                _ => signature += "n",
            }
        }
        signature += ")";
        Ok(Ast::Call(Id(signature.into()), args, src))
    }

    fn num(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        Ok({
            if self.current_token == OpnBrace { // TODO handle list literals
                self.braced()?
            }
            else if let Some(token) = self.consume_maybe(Num(0))? {
                Ast::Num(token, src)
            }
            else {
                match self.listref()? {
                    Some(list) => list,
                    _ => {
                        let id = self.consume(Id("identifier".into()))?;
                        if self.current_token == OpnBrace { // function call
                            match self.fun_call(None, id)? {
                                Ast::Call(token, ast, s) => Ast::Call(token, ast, src.up_to_end_of(s)),
                                _ => unreachable!()
                            }
                        }
                        else if self.consume_maybe(OpnSqr)?.is_some() { // refer to seq index
                            let index_expr = self.expr()?;
                            let src = src.up_to_end_of(self.current_src_ref);
                            self.consume(ClsSqr)?;
                            Ast::ReferSeqIndex(id_to_seq_id(&id), index_expr.into(), src)
                        }
                        else { // refer to an id
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
            return Ok(Ast::LeftUnaryOp(Sub, self.dotcall()?.into(), src.up_to_end_of(self.current_src_ref)));
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
            }
            else {
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
            }
            else {
                out = Ast::bin_op(Is, out, self.added()?);
            }
        }
        else if let Some(token) = self.consume_any_maybe(&[Gt, Lt, GtEq, LtEq])? {
            out = Ast::bin_op(token, out, self.added()?);
        }
        Ok(out)
    }

    fn inversed(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        if self.consume_maybe(Not)?.is_some() {
            Ok(Ast::LeftUnaryOp(Not, self.compared()?.into(), src.up_to_end_of(self.current_src_ref)))
        }
        else {
            self.compared()
        }
    }

    fn anded(&mut self) -> Res<Ast> {
        let mut out = self.inversed()?;
        while self.consume_maybe(And)?.is_some() {
            out = Ast::bin_op(And, out, self.inversed()?);
        }
        Ok(out)
    }

    fn ored(&mut self) -> Res<Ast> {
        let mut out = self.anded()?;
        while self.consume_maybe(Or)?.is_some() {
            out = Ast::bin_op(Or, out, self.anded()?);
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
    fn line_if_else(&mut self, scope: usize, mut allow: Rc<Vec<Token>>) -> Res<Ast> {
        let src = self.current_src_ref;
        let (expr, is_else) = {
            if self.consume_maybe(If)?.is_some() {
                (self.expr()?, false)
            }
            else {
                let src = src.up_to_end_of(self.current_src_ref);
                self.consume(Else)?;
                (
                    self.consume_maybe(If)?.map(|_| self.expr()).unwrap_or_else(
                        || Ok(Ast::Num(Num(1), src)),
                    )?,
                    true,
                )
            }
        };
        self.consume(Eol)?;
        if !allow.contains(&Else) {
            let mut new_allow: Vec<Token> = allow.as_ref().clone();
            new_allow.push(Else);
            allow = new_allow.into();
        }
        let block = self.lines_while_allowing(
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            allow,
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_end_of(self.current_src_ref))
                .describe("Parser: Expected line after `if,else` with exactly +1 indent"));
        }
        // else will be in unused_lines as they would mark the end of an if block
        if let Some(line) = self.unused_lines.pop() {
            if line.is_else_line() {
                return Ok(Ast::if_else(expr, block.unwrap(), line, is_else, src));
            }
            self.unused_lines.push(line);
        }
        Ok(Ast::just_if(expr, block.unwrap(), is_else, src))
    }

    // loop
    //     line+
    fn line_loop(&mut self, scope: usize) -> Res<Ast> {
        let src = self.current_src_ref;
        let (while_expr, for_stuff) = {
            if self.consume_maybe(While)?.is_some() {
                (self.expr()?, None)
            }
            else if self.consume_maybe(For)?.is_some() {
                let mut idx_id = Some(self.consume(Id("identifier".into()))?);
                let item_id = match self.consume_maybe(Comma)? {
                    Some(_) => self.consume(Id("identifier".into()))?,
                    _ => idx_id.take().unwrap()
                };

                self.consume(In)?;
                let list = self.list()?;
                (Ast::Empty(src.up_to_end_of(self.current_src_ref)), Some((idx_id, item_id, list)))
            }
            else {
                self.consume(Loop)?;
                (Ast::num(1, src.up_to_end_of(self.current_src_ref)), None)
            }
        };
        self.consume(Eol)?;
        let block = self.lines_while_allowing(
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            vec![Break, Continue].into(),
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_end_of(self.current_src_ref))
                .describe("Parser: Expected line after `loop,while,for` with exactly +1 indent"));
        }
        Ok(if let Some((idx_id, item_id, list)) = for_stuff {
            let src = src.up_to_end_of(list.src());
            Ast::ForIn(idx_id, item_id, list.into(), block.unwrap().into(), src)
        }
        else {
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
            _ => unreachable!()
        };
        let mut arg_ids = vec![];
        while let Some(mut arg) = self.consume_maybe(Id(id_name.clone()))? {
            if self.consume_maybe(Square)?.is_some() {
                arg = id_to_seq_id(&arg);
                signature += "l"
            }
            else {
                signature += "n";
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
            |l| match *l {
                Ast::Line(line_scope, ..) => line_scope > scope,
                _ => false,
            },
            vec![Return].into(),
        )?;
        if block.is_none() {
            return Err(BadderError::at(src.up_to_end_of(self.current_src_ref))
                .describe("Parser: Expected line after `fun` declaration with exactly +1 indent"));
        }
        Ok(Ast::AssignFun(Id(signature.into()), arg_ids, block.unwrap().into(), src))
    }

    /// return optional list match
    fn listref(&mut self) -> Res<Option<Ast>> {
        if self.consume_maybe(OpnBrace)?.is_some() {
            let list = self.list()?;
            self.consume(ClsBrace)?;
            return Ok(Some(list));
        }
        let src = self.current_src_ref;
        if let Id(..) = self.current_token {
            if self.lexer.peek()? == Square {
                let id = self.consume(Id("identifier".into()))?;
                self.consume(Square)?;
                return Ok(Some(Ast::ReferSeq(id_to_seq_id(&id), src.up_to_end_of(self.current_src_ref))));
            }
        }
        Ok(None)
    }

    fn list(&mut self) -> Res<Ast> {
        let src = self.current_src_ref;
        if let Some(list) = self.listref()? {
            return Ok(list);
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
        }
        else {
            let src = src.up_to(self.current_src_ref);
            Ast::AssignSeq(seq_id, Ast::Seq(vec![], src).into(), src)
        })
    }

    fn line_expr(&mut self, scope: usize, allow: Rc<Vec<Token>>) -> Res<Ast> {
        let src = self.current_src_ref;

        if allow.contains(&self.current_token) {
            if let Some(token) = self.consume_any_maybe(&[Break, Continue])? {
                return Ok(Ast::LoopNav(token, src));
            }
            if self.consume_maybe(Return)?.is_some() {
                let expr = {
                    if self.consume_maybe(Eol)?.is_none() {
                        let return_expr = self.expr()?;
                        self.consume(Eol)?;
                        return_expr
                    }
                    else {
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
                self.consume_any(&[Ass, Eol, Eof])?;
                let src = src.up_to(self.current_src_ref);
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
            if peek == OpnSqr { // ReferSeqIndex|ReassignSeqIndex
                let mut out = self.num()?;
                if self.consume_maybe(Ass)?.is_some() {
                    let expr = self.expr()?;
                    out = match out {
                        Ast::ReferSeqIndex(id, idx, src) => {
                            let src = src.up_to_end_of(expr.src());
                            Ast::ReassignSeqIndex(id, idx, expr.into(), src)
                        },
                        _ => {
                            return Err(BadderError::at(src.up_to_end_of(self.current_src_ref))
                                .describe(format!("Parser: expecting seq index ref id[expr], got {:?}",
                                                  out)));
                       }
                    }
                }
                return Ok(out);
            }
        }
        // If expr
        //     line+
        if self.current_token == If || allow.contains(&Else) && self.current_token == Else {
            return self.line_if_else(scope, allow);
        }
        // loop
        //     line+
        if [While, Loop, For].contains(&self.current_token) {
            return self.line_loop(scope);
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

    fn indented_line(&mut self, allow: Rc<Vec<Token>>) -> Res<Ast> {
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
                }, // reset scope, and skip empty lines
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

    fn lines_while_allowing<F>(&mut self, predicate: F, allow: Rc<Vec<Token>>)
        -> Res<Option<Ast>>
        where F: Fn(&Ast) -> bool
    {
        let mut all = vec![];
        let mut line = self.indented_line(allow.clone())?;
        while match line { Ast::Empty(..) => false, ref l => predicate(l) } {
            all.push(line);
            line = self.indented_line(allow.clone())?;
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

    fn lines_while<F>(&mut self, predicate: F) -> Res<Option<Ast>> where F: Fn(&Ast) -> bool {
        self.lines_while_allowing(predicate, vec![].into())
    }

    pub fn parse(&mut self) -> Res<Ast> {
        let lines = self.lines_while(|_| true)?;
        Ok(lines.unwrap_or(Ast::Empty(self.current_src_ref)))
    }
}

#[cfg(test)]
mod parser_test {
    extern crate pretty_env_logger;

    use super::*;

    macro_rules! src_eq {
        ($left:expr, $right:expr) => {{
            let src = $left.src();
            if src != $right {
                println!("Unexpected src for `{}`, `{:?}` != `{:?}`",
                         $left.debug_string(),
                         src,
                         $right);
                false
            }
            else { true }
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
                _ => panic!("Unexpected {}", $ast.debug_string())
            }
        };
        ($ast:ident = $pat:pat, src = $src:expr) => {{
            let src_eq = src_eq!($ast, $src);
            let out = match $ast {
                $pat => $ast,
                _ => panic!("Unexpected {}", $ast.debug_string())
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
                _ => panic!("Unexpected {}", $ast.debug_string())
            }
        }
    }

    macro_rules! expect_bin_op {
        ($ast:ident, src = $src:expr) => {{
            src_eq!($ast, $src); // just print
            match $ast {
                Ast::BinOp(_, left, right, src) => {
                    assert_eq!(src, $src);
                    (*left, *right)
                },
                _ => panic!("Unexpected {}", $ast.debug_string())
            }
        }}
    }

    macro_rules! expect_if_ast {
        ($ast:ident, src = $src:expr) => {{
            src_eq!($ast, $src); // just print
            match $ast {
                Ast::If(expr, block, elif, _, src) => {
                    assert_eq!(src, $src);
                    (*expr, block.as_ref().clone(), elif.map(|boxed| *boxed))
                }
                _ => panic!("Unexpected {}", $ast.debug_string())
            }
        }}
    }

    #[test]
    fn seq_src_ref() {
        let _ = pretty_env_logger::init();

        let mut ast = Parser::parse_str("        seq some_id[] = 1345, 2").unwrap();

        ast = *expect_ast!(ast = Ast::Line(2, ast, ..), src = SourceRef((1, 1), (1, 32)));
        ast = *expect_ast!(ast = Ast::AssignSeq(_, ast, ..), src = SourceRef((1, 9), (1, 32)));
        let seq_ast: Vec<Ast> = expect_ast!(ast = Ast::Seq(ast, ..), src = SourceRef((1, 25), (1, 32)));

        assert_eq!(seq_ast.len(), 2);
        assert_src_eq!(seq_ast[0], SourceRef((1, 25), (1, 29)));
        assert_src_eq!(seq_ast[1], SourceRef((1, 31), (1, 32)));
    }

    #[test]
    fn if_else_src_ref() {
        let _ = pretty_env_logger::init();

        let ast = Parser::parse_str(&vec![
            "var a = 123",
            "if a > 100",
            "    a *= 2",
            "else if a > 50",
            "    a = a / 3",
            "else",
            "    a += 32"].join("\n")
        ).unwrap();

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
        let (expr, block, next) = expect_if_ast!(ast, src = SourceRef((6, 1), (6, 5)));
        assert_eq!(next, None);
    }

    #[test]
    fn fun_src_ref() {
        let _ = pretty_env_logger::init();

        let ast = Parser::parse_str(&vec![
            "fun double(x)",
            "    return x * 2 # used return to test it, also this comment",
            "double(2.double())"].join("\n")
        ).unwrap();

        let (ast, next) = expect_line_pair!(ast);

        let ast = *expect_ast!(ast = Ast::Line(0, ast, ..), src = SourceRef((1, 1), (1, 14)));
        let ref ast = *expect_ast!(ast = Ast::AssignFun(.., ast, _),
            src = SourceRef((1, 1), (1, 14)));
        let ref ast = **expect_ast!(ast = &Ast::Line(1, ref ast, ..),
            src = SourceRef((2, 1), (2, 17)));
        expect_ast!(ast = &Ast::Return(ref ast, ..), src = SourceRef((2, 5), (2, 17)));

        let ast = *expect_ast!(next = Ast::Line(0, next, ..), src = SourceRef((3, 1), (3, 19)));
        let ast = expect_ast!(ast = Ast::Call(_, ast, ..), src = SourceRef((3, 1), (3, 19))).remove(0);
        let ast = expect_ast!(ast = Ast::Call(_, ast, ..), src = SourceRef((3, 8), (3, 18))).remove(0);
        expect_ast!(ast = Ast::Num(Num(2), ..), src = SourceRef((3, 8), (3, 9)));
    }
}
