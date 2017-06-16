use super::{Int, Res};
use lexer::*;
use lexer::Token::*;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use string_cache::DefaultAtom as Atom;

/// Abstract syntax tree
#[derive(PartialEq, Clone)]
pub enum Ast {
    Num(Token),
    BinOp(Token, Box<Ast>, Box<Ast>),
    LeftUnaryOp(Token, Box<Ast>),
    /// Assign(variable_id, ast)
    Assign(Token, Box<Ast>),
    Reassign(Token, Box<Ast>),
    /// Refer(variable_id)
    Refer(Token),
    /// If(if-expr, resultant-block, Line(If), is_else)
    If(Box<Ast>, Arc<Ast>, Option<Box<Ast>>, bool),
    /// While(if-expr, resultant-block)
    While(Box<Ast>, Arc<Ast>),
    /// ForIn(index-id, item-id, list, block)
    ForIn(Option<Token>, Token, Box<Ast>, Arc<Ast>),
    /// LoopNav(break|continue)
    LoopNav(Token),
    /// Fun(function-id, args, block) defines a function
    Fun(Token, Vec<Token>, Arc<Ast>),
    // Return(expr) used inside function blocks
    Return(Box<Ast>),
    /// Call(function-id, args)
    Call(Token, Vec<Ast>),
    /// Literal sequence of expressions
    Seq(Vec<Ast>),
    /// AssignSeq(seq-id, Seq)
    AssignSeq(Token, Box<Ast>),
    /// ReferSeq(seq-id) refer to a sequence
    ReferSeq(Token),
    /// ReferSeqIndex(seq-id, index)
    ReferSeqIndex(Token, Box<Ast>),
    /// ReassignSeqIndex(seq-id, index, val)
    ReassignSeqIndex(Token, Box<Ast>, Box<Ast>),
    /// Line(scope, expr)
    Line(usize, Box<Ast>),
    /// LinePair(line, next_line)
    LinePair(Box<Ast>, Box<Ast>),
    Empty,
}

impl fmt::Debug for Ast {
    /// Safe for large recursive structures
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ast::Num(ref t) => write!(f, "Num({:?})", t),
            Ast::BinOp(ref t, ref l, ref r) => write!(f, "BinOp({:?},{:?},{:?})", t, l, r),
            Ast::LeftUnaryOp(ref t, ref expr) => write!(f, "LeftUnaryOp({:?},{:?})", t, expr),
            Ast::Assign(ref t, ref expr) => write!(f, "Assign({:?},{:?})", t, expr),
            Ast::Reassign(ref t, ref expr) => write!(f, "Reassign({:?},{:?})", t, expr),
            Ast::Refer(ref t) => write!(f, "Refer({:?})", t),
            Ast::If(ref bool_expr, _, _, is_else) => {
                write!(f, "{}If({:?},_,_)", if is_else { "Else" } else { "" }, bool_expr)
            },
            Ast::While(ref bool_expr, _) => write!(f, "While({:?},_)", bool_expr),
            Ast::ForIn(None, ref id, ref list, ..) => write!(f, "ForIn({:?}:{:?})", id, list),
            Ast::ForIn(Some(ref idx_id), ref id, ref list, ..) => {
                write!(f, "ForIn({:?}{:?}:{:?})", idx_id, id, list)
            },
            Ast::LoopNav(ref t) => write!(f, "LoopNav({:?})", t),
            Ast::Fun(ref t, ref args, _) => write!(f, "Fun({:?},{:?},_)", t, args),
            Ast::Return(ref val) => write!(f, "Return({:?})", val),
            Ast::Call(ref t, ref args) => write!(f, "Call({:?},{:?})", t, args),
            Ast::ReferSeq(ref t) => write!(f, "ReferSeq({:?})", t),
            Ast::Seq(ref exprs) => write!(f, "Seq({:?})", exprs),
            Ast::AssignSeq(ref t, ref seq) => write!(f, "AssignSeq({:?},{:?})", t, seq),
            Ast::ReferSeqIndex(ref t, ref i) => write!(f, "ReferSeqIndex({:?},{:?})", t, i),
            Ast::ReassignSeqIndex(ref t, ref i, ref val) => {
                write!(f, "ReassignSeqIndex({:?},{:?},{:?})", t, i, val)
            },
            Ast::Line(ref scope, _) => write!(f, "Line({},_)", scope),
            Ast::LinePair(..) => write!(f, "LinePair(_,_)"),
            Ast::Empty => write!(f, "Empty"),
        }
    }
}

impl Ast {
    pub fn bin_op<A: Into<Box<Ast>>>(token: Token, left: A, right: A) -> Ast {
        Ast::BinOp(token, left.into(), right.into())
    }

    pub fn left_unary_op<A: Into<Box<Ast>>>(token: Token, node: A) -> Ast {
        Ast::LeftUnaryOp(token, node.into())
    }

    pub fn line<A: Into<Box<Ast>>>(scope: usize, expr: A) -> Ast {
        Ast::Line(scope, expr.into())
    }

    pub fn line_pair<A: Into<Box<Ast>>>(before: A, after: A) -> Ast {
        let line = before.into();
        if let Ast::LinePair(..) = *line {
            panic!("LinePair left val must not be a LinePair");
        }
        Ast::LinePair(line, after.into())
    }

    pub fn just_if(expr: Ast, block: Ast, is_else: bool) -> Ast {
        Ast::If(expr.into(), block.into(), None, is_else)
    }

    pub fn if_else(expr: Ast, block: Ast, else_if: Ast, is_else: bool) -> Ast {
        Ast::If(expr.into(), block.into(), Some(else_if.into()), is_else)
    }

    pub fn num(n: Int) -> Ast {
        Ast::Num(Num(n))
    }

    pub fn debug_string(&self) -> String {
        match self {
            pair @ &Ast::LinePair(..) => {
                let mut next = pair;
                let mut out = String::new();
                while let &Ast::LinePair(ref l1, ref l2) = next {
                    out = out + &l1.debug_string() + "\n";
                    next = l2;
                }
                out + &next.debug_string()
            },
            &Ast::Line(scope, ref expr) =>
                format!("-{}{}> {}", scope, "-".repeat(scope), expr.debug_string()),
            &Ast::If(ref expr, ref block, None, _) => format!("If({})\n{}", expr.debug_string(), block.debug_string()),
            &Ast::If(ref expr, ref block, Some(ref elif), _) => {
                format!("If({})\n{}\nElse{}",
                        expr.debug_string(),
                        block.debug_string(),
                        elif.debug_string())
            },
            &Ast::While(ref expr, ref block) => format!("While({})\n{}", expr.debug_string(), block.debug_string()),
            &Ast::Fun(ref id, ref args, ref block) => format!("DefFun({:?}, {:?})\n{}", id, args, block.debug_string()),
            x => format!("{:?}", x),
        }
    }

    fn is_else_line(&self) -> bool {
        if let &Ast::Line(_, ref ast) = self {
            if let Ast::If(_, _, _, is_else) = **ast {
                return is_else;
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    /// stack of 'next lines' that we want to process later with earlier lines at the top
    unused_lines: Vec<Ast>,
}

impl<'a> Parser<'a> {
    pub fn parse_str(code: &'a str) -> Res<Ast> {
        let mut lexer = Lexer::new(code);
        let first = lexer.next_token()?;
        Parser {
            lexer,
            current_token: first,
            unused_lines: Vec::new(),
        }.parse()
    }

    fn next_token(&mut self) -> Res<&Token> {
        self.current_token = self.lexer.next_token()?;
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
            Err(format!(
                "Parser: {} Expected `{}` got {}",
                self.lexer.cursor_debug(),
                expected,
                self.current_token.long_debug()
            ))
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
        let mut args = left.map(|a| vec![a]).unwrap_or_else(|| vec![]);
        self.consume(OpnBrace)?;
        while self.current_token != ClsBrace {
            args.push(self.expr()?);
            if self.consume_maybe(Comma)?.is_none() {
                break;
            }
        }
        self.consume(ClsBrace)?;
        Ok(Ast::Call(id, args))
    }

    fn num(&mut self) -> Res<Ast> {
        Ok({
            if self.current_token == OpnBrace {
                self.braced()?
            }
            else if let Some(token) = self.consume_maybe(Num(0))? {
                Ast::Num(token)
            }
            else {
                let id = self.consume(Id("identifier".into()))?;
                if self.current_token == OpnBrace { // function call
                    self.fun_call(None, id)?
                }
                else if self.consume_maybe(OpnSqr)?.is_some() { // refer to seq index
                    let index_expr = self.expr()?;
                    self.consume(ClsSqr)?;
                    Ast::ReferSeqIndex(id, index_expr.into())
                }
                else { // refer to an id
                    Ast::Refer(id)
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
        if self.current_token == Sub {
            self.next_token()?;
            return Ok(Ast::left_unary_op(Sub, self.dotcall()?));
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
        let mut out = self.added()?;
        if self.consume_maybe(Is)?.is_some() {
            if self.consume_maybe(Not)?.is_some() {
                let is = Ast::bin_op(Is, out, self.added()?);
                out = Ast::left_unary_op(Not, is);
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
        if self.consume_maybe(Not)?.is_some() {
            Ok(Ast::left_unary_op(Not, self.compared()?))
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
        let (expr, is_else) = {
            if self.consume_maybe(If)?.is_some() {
                (self.expr()?, false)
            }
            else {
                self.consume(Else)?;
                (
                    self.consume_maybe(If)?.map(|_| self.expr()).unwrap_or_else(
                        || Ok(Ast::Num(Num(1))),
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
            |l| match l {
                &Ast::Line(line_scope, _) => line_scope > scope,
                _ => false,
            },
            allow,
        )?;
        if block.is_none() {
            return Err(format!(
                "Parser: {} Expected line after `if,else` with exactly +1 indent",
                self.lexer.cursor_debug()
            )); // TODO line numbers dodgy from unused_lines processing
        }
        // else will be in unused_lines as they would mark the end of an if block
        if let Some(line) = self.unused_lines.pop() {
            if line.is_else_line() {
                return Ok(Ast::if_else(expr, block.unwrap(), line, is_else));
            }
            self.unused_lines.push(line);
        }
        Ok(Ast::just_if(expr, block.unwrap(), is_else))
    }

    // loop
    //     line+
    fn line_loop(&mut self, scope: usize) -> Res<Ast> {
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
                (Ast::Empty, Some((idx_id, item_id, list)))
            }
            else {
                self.consume(Loop)?;
                (Ast::num(1), None)
            }
        };
        self.consume(Eol)?;
        let block = self.lines_while_allowing(
            |l| match l {
                &Ast::Line(line_scope, _) => line_scope > scope,
                _ => false,
            },
            vec![Break, Continue].into(),
        )?;
        if block.is_none() {
            return Err(format!(
                "Parser: {} Expected line after `loop,while,for` with exactly +1 indent",
                self.lexer.cursor_debug()
            )); // TODO line numbers dodgy from unused_lines processing
        }
        Ok(if let Some((idx_id, item_id, list)) = for_stuff {
            Ast::ForIn(idx_id, item_id, list.into(), block.unwrap().into())
        }
        else {
            Ast::While(while_expr.into(), block.unwrap().into())
        })
    }

    // fun id()
    //     line+
    fn line_fun(&mut self, scope: usize) -> Res<Ast> {
        self.consume(Fun)?;
        let id_name: Atom = "identifier".into();
        let id = self.consume(Id(id_name.clone()))?;
        self.consume(OpnBrace)?;
        let mut arg_ids = vec![];
        while let Some(arg) = self.consume_maybe(Id(id_name.clone()))? {
            arg_ids.push(arg);
            if self.consume_maybe(Comma)?.is_none() {
                break;
            }
        }
        self.consume(ClsBrace)?;
        self.consume(Eol)?;
        let block = self.lines_while_allowing(
            |l| match l {
                &Ast::Line(line_scope, _) => line_scope > scope,
                _ => false,
            },
            vec![Return].into(),
        )?;
        if block.is_none() {
            return Err(format!(
                "Parser: {} Expected line after `fun` declaration with exactly +1 indent",
                self.lexer.cursor_debug()
            )); // TODO line numbers dodgy from unused_lines processing
        }
        Ok(Ast::Fun(id, arg_ids, block.unwrap().into()))
    }

    fn list(&mut self) -> Res<Ast> {
        if self.consume_maybe(OpnBrace)?.is_some() {
            let list = self.list()?;
            self.consume(ClsBrace)?;
            return Ok(list);
        }
        let mut exprs = vec![match self.expr()? {
            a @ Ast::Refer(_) => {
                if self.consume_maybe(Square)?.is_some() { // seq reference
                    return Ok(match a {
                        Ast::Refer(id) => Ast::ReferSeq(id),
                        _ => unreachable!()
                    });
                }
                a
            },
            expr => expr
        }];
        while self.consume_maybe(Comma)?.is_some() {
            exprs.push(self.expr()?);
        }
        Ok(Ast::Seq(exprs))
    }

    // seq id[] = expr,expr,
    fn line_seq(&mut self) -> Res<Ast> {
        self.consume(Seq)?;
        let seq_id = self.consume(Id("identifier".into()))?;
        self.consume(Square)?;
        Ok(if self.consume_maybe(Ass)?.is_some() {
            Ast::AssignSeq(seq_id, self.list()?.into())
        }
        else {
            Ast::AssignSeq(seq_id, Ast::Seq(vec![]).into())
        })
    }

    fn line_expr(&mut self, scope: usize, allow: Rc<Vec<Token>>) -> Res<Ast> {
        if allow.contains(&self.current_token) {
            if let Some(token) = self.consume_any_maybe(&[Break, Continue])? {
                return Ok(Ast::LoopNav(token));
            }
            if self.consume_maybe(Return)?.is_some() {
                let expr = {
                    if self.consume_maybe(Eol)?.is_none() {
                        let return_expr = self.expr()?;
                        self.consume(Eol)?;
                        return_expr
                    }
                    else {
                        Ast::num(0)
                    }
                };
                return Ok(Ast::Return(expr.into()));
            }
        }

        // var id = expr
        if self.consume_maybe(Var)?.is_some() {
            let id = self.consume(Id("identifier".into()))?;
            if self.consume_maybe(Ass)?.is_none() {
                self.consume_any(&[Ass, Eol, Eof])?;
                return Ok(Ast::Assign(id, Ast::Num(Num(0)).into()));
            }
            let expr = self.expr()?;
            self.consume_any(&[Eol, Eof])?;
            return Ok(Ast::Assign(id, expr.into()));
        }
        // id = expr
        if let Id(_) = self.current_token {
            let peek = self.lexer.peek()?;
            if peek == Ass {
                let id = self.consume(Id("identifier".into()))?;
                self.consume(Ass)?;
                let expr = self.expr()?;
                self.consume_any(&[Eol, Eof])?;
                return Ok(Ast::Reassign(id, expr.into()));
            }
            if let OpAss(op) = peek {
                let id = self.consume(Id("identifier".into()))?;
                self.next_token()?;
                let expr = self.expr()?;
                self.consume_any(&[Eol, Eof])?;
                // v *= expr  ->  v = v * expr
                return Ok(Ast::Reassign(id.clone(), Ast::bin_op(*op, Ast::Refer(id), expr).into()));
            }
            if peek == OpnSqr { // ReferSeqIndex|ReassignSeqIndex
                // let id = self.consume(Id("identifier".into()))?;
                // self.consume(OpnSqr)?;
                // let index_expr = self.expr()?;
                // self.consume(ClsSqr)?;
                let mut out = self.num()?;
                if self.consume_maybe(Ass)?.is_some() {
                    let expr = self.expr()?;
                    out = match out {
                        Ast::ReferSeqIndex(id, idx) => Ast::ReassignSeqIndex(id, idx, expr.into()),
                        _ => {
                            return Err(format!("Parser: expecting seq index ref id[expr], got {:?}",
                                               out));
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
        Ok(Ast::Empty)
    }

    fn indented_line(&mut self, allow: Rc<Vec<Token>>) -> Res<Ast> {
        if let Some(line) = self.unused_lines.pop() {
            return Ok(line);
        }

        let mut scope = 0;
        while let Some(token) = self.consume_any_maybe(&[Eol, Indent(0)])? {
            match token {
                Indent(x) => scope = x,
                Eol => scope = 0, // reset scope, and skip empty lines
                _ => unreachable!(),
            };
        }
        Ok(match self.line_expr(scope, allow)? {
            Ast::Empty => Ast::Empty,
            ast => Ast::Line(scope, ast.into()),
        })
    }

    fn lines_while_allowing<F>(&mut self, predicate: F, allow: Rc<Vec<Token>>)
        -> Res<Option<Ast>>
        where F: Fn(&Ast) -> bool
    {
        let mut all = vec![];
        let mut line = self.indented_line(allow.clone())?;
        while line != Ast::Empty && predicate(&line) {
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
        Ok(lines.unwrap_or(Ast::Empty))
    }
}
