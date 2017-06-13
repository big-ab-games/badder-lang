// use std::collections::HashMap;
use lexer::*;
use lexer::Token::*;
use super::{Res, Int};

/// Abstract syntax tree
#[derive(Debug, PartialEq, Clone)]
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
    If(Box<Ast>, Box<Ast>, Option<Box<Ast>>, bool),
    /// While(if-expr, resultant-block)
    While(Box<Ast>, Box<Ast>),
    /// LoopNav(break|continue)
    LoopNav(Token),
    /// Line(scope, expr)
    Line(usize, Box<Ast>),
    /// LinePair(line, next_line)
    LinePair(Box<Ast>, Box<Ast>),
    Empty,
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
        if let Ast::LinePair(_, _) = *line {
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
            &Ast::LinePair(ref line, ref next) => line.debug_string() + "\n" + &next.debug_string(),
            &Ast::Line(scope, ref expr) => format!("-{}{}> {}",
                scope, "-".repeat(scope), expr.debug_string()),
            &Ast::If(ref expr, ref block, None, _) => format!("If({})\n{}",
                expr.debug_string(), block.debug_string()),
            &Ast::If(ref expr, ref block, Some(ref elif), _) => format!("If({})\n{}\nElse{}",
                expr.debug_string(), block.debug_string(), elif.debug_string()),
            &Ast::While(ref expr, ref block) => format!("While({})\n{}",
                expr.debug_string(), block.debug_string()),
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
    pub fn new(code: &'a str) -> Res<Parser<'a>> {
        let mut lexer = Lexer::new(code);
        let first = lexer.next_token()?;
        Ok(Parser {
            lexer,
            current_token: first,
            unused_lines: Vec::new(),
        })
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
            let expected = types.iter()
                .map(|t| match t {
                    &Num(_) => "0-9".to_string(),
                    token => format!("{:?}", token)
                })
                .collect::<Vec<String>>()
                .join(",");
            Err(format!("Parser: {} Expected `{}` got {}",
                self.lexer.cursor_debug(), expected, self.current_token.long_debug()))
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

    fn num(&mut self) -> Res<Ast> {
        if self.current_token == OpnBrace {
            return self.braced();
        }
        Ok(match self.consume_any(&[Num(0), Id("identifier".into())])? {
            n @ Num(_) => Ast::Num(n),
            id @ Id(_) => Ast::Refer(id),
            _ => unreachable!(),
        })
    }

    fn signed(&mut self) -> Res<Ast> {
        if self.current_token == Sub {
            self.next_token()?;
            return Ok(Ast::left_unary_op(Sub, self.num()?));
        }
        self.num()
    }

    fn divided(&mut self) -> Res<Ast> {
        let mut out = self.signed()?;
        while self.consume_maybe(Div)?.is_some() {
            out = Ast::bin_op(Div, out, self.signed()?);
        }
        Ok(out)
    }

    fn timesed(&mut self) -> Res<Ast> {
        let mut out = self.divided()?;
        while let Some(token) = self.consume_any_maybe(&[Mul, Mod])? {
            out = Ast::bin_op(token, out, self.divided()?);
        }
        Ok(out)
    }

    fn added(&mut self) -> Res<Ast> {
        let mut out = self.timesed()?;
        while let Some(token) = self.consume_any_maybe(&[Pls, Sub])? {
            if token == Pls {
                out = Ast::bin_op(Pls, out, self.timesed()?);
            }
            else {
                out = Ast::bin_op(Sub, out, self.timesed()?);
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
    fn line_if_else(&mut self, scope: usize, mut allow: Vec<Token>) -> Res<Ast> {
        let (expr, is_else) = {
            if self.consume_maybe(If)?.is_some() {
                (self.expr()?, false)
            }
            else {
                self.consume(Else)?;
                (self.consume_maybe(If)?
                    .map(|_| self.expr())
                    .unwrap_or_else(|| Ok(Ast::Num(Num(1))))?, true)
            }
        };
        self.consume(Eol)?;
        if !allow.contains(&Else) {
            allow.push(Else);
        }
        let block = self.lines_while_allowing(|l| match l {
            &Ast::Line(line_scope, _) => line_scope > scope,
            _ => false
        }, allow)?;
        if block.is_none() {
            return Err(format!("Parser: {} Expected line after `if,else` with exactly +1 indent",
                self.lexer.cursor_debug())); // TODO line numbers dodgy from unused_lines processing
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
        let expr = {
            if self.consume_maybe(While)?.is_some() {
                self.expr()?
            }
            else {
                self.consume(Loop)?;
                Ast::num(1)
            }
        };
        self.consume(Eol)?;
        let block = self.lines_while_allowing(|l| match l {
            &Ast::Line(line_scope, _) => line_scope > scope,
            _ => false
        }, vec!(Break, Continue))?;
        if block.is_none() {
            return Err(format!("Parser: {} Expected line after `loop,while,for` with exactly +1 indent",
                self.lexer.cursor_debug())); // TODO line numbers dodgy from unused_lines processing
        }
        Ok(Ast::While(expr.into(), block.unwrap().into()))
    }

    fn line_expr(&mut self, scope: usize, allow: Vec<Token>) -> Res<Ast> {
        if let Some(token) = self.consume_any_maybe(&[Break, Continue])? {
            return Ok(Ast::LoopNav(token));
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
        }
        // If expr
        //     line+
        if self.current_token == If || allow.contains(&Else) && self.current_token == Else {
            return self.line_if_else(scope, allow);
        }
        // loop
        //     line+
        if self.current_token == Loop || self.current_token == While {
            return self.line_loop(scope);
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

    fn indented_line(&mut self, allow: Vec<Token>) -> Res<Ast> {
        if let Some(line) = self.unused_lines.pop() {
            return Ok(line);
        }

        let mut scope = 0;
        while let Some(token) = self.consume_any_maybe(&[Eol, Indent(0)])? {
            match token {
                Indent(x) => scope = x,
                Eol => scope = 0, // reset scope, and skip empty lines
                _ => unreachable!()
            };
        }
        Ok(match self.line_expr(scope, allow)? {
            Ast::Empty => Ast::Empty,
            ast => Ast::Line(scope, ast.into())
        })
    }

    fn lines_while_allowing<F>(&mut self, predicate: F, allow: Vec<Token>) -> Res<Option<Ast>>
        where F: Fn(&Ast) -> bool
    {
        let line = self.indented_line(allow.clone())?;
        if line == Ast::Empty || !predicate(&line) {
            self.unused_lines.push(line);
            return Ok(None);
        }
        Ok(match self.lines_while_allowing(predicate, allow)? {
            None => Some(line),
            Some(next) => Some(Ast::line_pair(line, next)),
        })
    }

    fn lines_while<F>(&mut self, predicate: F) -> Res<Option<Ast>>
        where F: Fn(&Ast) -> bool
    {
        self.lines_while_allowing(predicate, vec!())
    }

    pub fn parse(&mut self) -> Res<Ast> {
        let lines = self.lines_while(|_| true)?;
        Ok(lines.unwrap_or(Ast::Empty))
    }
}
