use std::str::Chars;
use std::fmt;
use super::{Res, Int};

#[derive(PartialEq, Clone, Copy)]
pub enum Token {
    Num(Int),
    Pls,
    Sub,
    Mul,
    Div,
    Eof,
    OpnBrace,
    ClsBrace,
}

use lexer::Token::*;

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
pub struct Lexer<'a> {
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

    pub fn next_token(&mut self) -> Res<Token> {
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
