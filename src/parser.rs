use lexer::*;
use lexer::Token::*;
use super::Res;

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Num(Token),
    BinOp(Token, Box<Ast>, Box<Ast>),
    LeftUnaryOp(Token, Box<Ast>),
}

impl Ast {
    pub fn bin_op<A: Into<Box<Ast>>>(token: Token, left: A, right: A) -> Ast {
        Ast::BinOp(token, left.into(), right.into())
    }

    pub fn left_unary_op<A: Into<Box<Ast>>>(token: Token, node: A) -> Ast {
        Ast::LeftUnaryOp(token, node.into())
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Res<Parser<'a>> {
        let mut lexer = Lexer::new(code);
        let first = lexer.next_token()?;
        Ok(Parser {
            lexer,
            current_token: first,
        })
    }

    fn next_token(&mut self) -> Res<Token> {
        self.current_token = self.lexer.next_token()?;
        Ok(self.current_token)
    }

    fn consume(&mut self, types: &[Token]) -> Res<Token> {
        if types.iter().any(|t| t.matches(self.current_token)) {
            let res = Ok(self.current_token);
            self.next_token()?;
            res
        }
        else {
            let expected = types.iter()
                .map(|t| match *t {
                    Num(_) => "0-9".to_string(),
                    token => format!("{:?}", token)
                })
                .collect::<Vec<String>>()
                .join(",");
            Err(format!("Parser: Expected {} got {:?}", expected, self.current_token))
        }
    }

    fn consume_maybe(&mut self, types: &[Token]) -> Res<Option<Token>> {
        if types.iter().any(|t| t.matches(self.current_token)) {
            let res = Ok(Some(self.current_token));
            self.next_token()?;
            return res;
        }
        Ok(None)
    }

    fn num(&mut self) -> Res<Ast> {
        if self.current_token == OpnBrace {
            return self.braced();
        }
        Ok(match self.consume(&[Num(0)])? {
            n @ Num(_) => Ast::Num(n),
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
        while self.consume_maybe(&[Div])?.is_some() {
            out = Ast::bin_op(Div, out, self.signed()?);
        }
        Ok(out)
    }

    fn timesed(&mut self) -> Res<Ast> {
        let mut out = self.divided()?;
        while self.consume_maybe(&[Mul])?.is_some() {
            out = Ast::bin_op(Mul, out, self.divided()?);
        }
        Ok(out)
    }

    fn added(&mut self) -> Res<Ast> {
        let mut out = self.timesed()?;
        while let Some(token) = self.consume_maybe(&[Pls, Sub])? {
            if token == Pls {
                out = Ast::bin_op(Pls, out, self.timesed()?);
            }
            else {
                out = Ast::bin_op(Sub, out, self.timesed()?);
            }
        }
        Ok(out)
    }

    fn braced(&mut self) -> Res<Ast> {
        if self.consume_maybe(&[OpnBrace])?.is_some() {
            let out = self.added()?;
            self.consume(&[ClsBrace])?;
            return Ok(out);
        }
        self.added()
    }

    pub fn parse(&mut self) -> Res<Ast> {
        let out = self.added()?;
        self.consume(&[Eof])?;
        Ok(out)
    }
}
