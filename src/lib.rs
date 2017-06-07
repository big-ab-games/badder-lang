use std::str::Chars;
use std::fmt;

type Res<T> = Result<T, String>;
type Int = i32;

#[derive(PartialEq, Clone, Copy)]
enum Token {
    Num(Int),
    Pls,
    Sub,
    Mul,
    Div,
    Eof,
    OpnBrace,
    ClsBrace,
}

use Token::*;

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Num(x) => write!(f, "{}", x),
            Pls => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Eof => write!(f, "Eof"),
            OpnBrace => write!(f, "("),
            ClsBrace => write!(f, ")"),
        }
    }
}

impl Token {
    pub fn parse(c: char) -> Option<Token> {
        match c {
            '-' => Some(Sub),
            '+' => Some(Pls),
            '*' => Some(Mul),
            '/' => Some(Div),
            '(' => Some(OpnBrace),
            ')' => Some(ClsBrace),
            _ => None,
        }
    }

    pub fn matches(&self, token: Token) -> bool {
        match *self {
            Num(_) => if let Num(_) = token { true } else { false },
            me => me == token,
        }
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    chars: Chars<'a>,
    current_char: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        let mut chars = code.chars();
        let first = chars.next();
        Lexer {
            chars,
            current_char: first
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.next_char() {
            if !c.is_whitespace() {
                break
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current_char = self.chars.next();
        self.current_char
    }

    fn next_token(&mut self) -> Res<Token> {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if c.is_digit(10) {
                let mut number_str = c.to_string();
                while let Some(c) = self.next_char(){
                    if c.is_digit(10) {
                        number_str.push(c);
                    }
                    else {
                        break;
                    }
                }
                return Ok(Token::Num(number_str.parse().expect("parse to integer")))
            }

            if let Some(token) = Token::parse(c) {
                self.next_char();
                return Ok(token);
            }

            return Err(format!("Lexer: Unexpected char: {}", c));
        }

        Ok(Token::Eof)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Ast {
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
struct Parser<'a> {
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

fn interpret(ast: Ast) -> Res<Int> {
    match ast {
        Ast::Num(Num(x)) => Ok(x),
        Ast::BinOp(token, left, right) => match token {
            Pls => Ok(interpret(*left)? + interpret(*right)?),
            Sub => Ok(interpret(*left)? - interpret(*right)?),
            Mul => Ok(interpret(*left)? * interpret(*right)?),
            Div => Ok(interpret(*left)? / interpret(*right)?),
            _ => Err(format!("Interpreter: Unexpected BinOp token {:?}", token)),
        },
        Ast::LeftUnaryOp(Sub, val) => Ok(-interpret(*val)?),
        _ => Err(format!("Interpreter: Unexpected Ast {:?}", ast)),
    }
}


pub fn eval(code: &str) -> Res<Int> {
    interpret(Parser::new(code)?.parse()?)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn num() {
        assert_eq!(eval("0"), Ok(0));
        assert_eq!(eval("123"), Ok(123));
    }

    #[test]
    fn negative() {
        assert_eq!(eval("-235"), Ok(-235));
    }

    #[test]
    fn double_negative_err() {
        let out = eval("--235");
        assert!(out.is_err(), format!("Unexpected {:?}", out));
        if let Err(reason) = out {
            assert!(reason.contains("-"), format!("Unexpected reason {}", reason));
        }
    }

    #[test]
    fn negative_brace() {
        assert_eq!(eval("-(-45)"), Ok(45));
    }

    #[test]
    fn plus() {
        assert_eq!(eval("12 + 2"), Ok(14));
        assert_eq!(eval("-12 + 2 + 23"), Ok(13));
    }

    #[test]
    fn subtract() {
        assert_eq!(eval("12 - 2"), Ok(10));
        assert_eq!(eval("-12 - 2 - 4"), Ok(-18));
    }

    #[test]
    fn plus_sub() {
        assert_eq!(eval("12 - 2 + 3"), Ok(13));
        assert_eq!(eval("-12 + 2 - 4"), Ok(-14));
    }

    #[test]
    fn multiply() {
        assert_eq!(eval("3 * 4"), Ok(12));
        assert_eq!(eval("4 * -5 * 3"), Ok(-60));
    }

    #[test]
    fn divide() {
        assert_eq!(eval("5 / 2"), Ok(2)); // integer divide
        assert_eq!(eval("-12 / 2 / 2"), Ok(-3));
    }

    #[test]
    fn precidence() {
        assert_eq!(eval("3 + 6 / 2 * 7 + 1"), Ok(25));
    }

    #[test]
    fn negative_mixed_in() {
        assert_eq!(eval("3 + 6 / 2 * -7 + 1"), Ok(-17));
    }

    #[test]
    fn outer_brackets() {
        assert_eq!(eval("(7 + 3 * 4)"), Ok(19));
    }

    #[test]
    fn brackets() {
        assert_eq!(eval("7 + 3 * (10 / (12 / (3 + 1) - 1))"), Ok(22));
    }

    #[test]
    fn redundant_brackets() {
        assert_eq!(eval("7 + (((3 + 2)))"), Ok(12));
    }
}
