use std::str::Chars;
use std::iter::Peekable;
use std::fmt;
use super::{Res, Int};

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Num(Int),
    Id(String),

    // Operators
    Pls,
    Sub,
    Mul,
    Div,
    OpnBrace,
    ClsBrace,
    Ass,
    OpAss(Box<Token>),

    // End of things
    Eol,
    Eof,

    /// Keywords
    Var,
    Def,
    If,
    Else,
    And,
    Or,
    Not,
    Is,
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
            OpnBrace => write!(f, "("),
            ClsBrace => write!(f, ")"),
            Ass => write!(f, "="),
            OpAss(ref op) => write!(f, "{:?}=", *op),
            Eol => write!(f, "Eol"),
            Eof => write!(f, "Eof"),
            Var => write!(f, "var"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Not => write!(f, "not"),
            Is => write!(f, "is"),
            Def => write!(f, "def"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Id(ref id) => write!(f, "{}", id),
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
            '=' => Some(Ass),
            _ => None,
        }
    }

    pub fn parse_id(id: String) -> Token {
        match id.as_str() {
            "var" => Var,
            "if" => If,
            "else" => Else,
            "not" => Not,
            "is" => Is,
            "def" => Def,
            "and" => And,
            "or" => Or,
            _ => Id(id),
        }
    }

    fn is_valid_for_op_ass(&self) -> bool {
        match *self {
            Pls|Sub|Mul|Div => true,
            _ => false,
        }
    }

    pub fn matches(&self, token: &Token) -> bool {
        match *self {
            Num(_) => if let Num(_) = *token { true } else { false },
            Id(_) =>  if let Id(_) = *token { true } else { false },
            ref me => me == token,
        }
    }

    pub fn long_debug(&self) -> String {
        match *self {
            Num(x) => format!("number {}", x),
            Id(ref id) => format!("id `{}`", id),
            Pls|Sub|Mul|Div|OpnBrace|ClsBrace|Ass|OpAss(_) => format!("operator {:?}", self),
            _ => format!("keyword `{:?}`", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    current_char: Option<char>,
}

#[inline]
fn id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[inline]
fn junkspace(c: char) -> bool {
    c.is_whitespace() && c != '\n'
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        let mut chars = code.chars().peekable();
        let first = chars.next();
        Lexer {
            chars,
            current_char: first
        }
    }

    fn skip_junkspace(&mut self) {
        while let Some(c) = self.next_char() {
            if !junkspace(c) {
                break
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current_char = self.chars.next();
        self.current_char
    }

    /// Attempts to return next token without advancing, has limitations
    /// Nums & Ids with only contain their first character
    pub fn peek(&mut self) -> Res<Token> {
        while let Some(c) = self.current_char {
            if junkspace(c) {
                self.skip_junkspace();
                continue;
            }

            if c == '\n' {
                return Ok(Eol);
            }

            if c == '#' {
                while let Some(c) = self.next_char() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }

            if c.is_digit(10) {
                return Ok(Num(c as i32));
            }

            // non-digit as here
            if id_char(c) {
                return Ok(Id(c.to_string()));
            }

            if let Some(token) = Token::parse(c) {
                if token.is_valid_for_op_ass() {
                    if let Some(&'=') = self.chars.peek() {
                        return Ok(OpAss(token.into()));
                    }
                }
                return Ok(token);
            }

            return Err(format!("Lexer: Unexpected char: {}", c));
        }

        Ok(Eof)
    }

    pub fn next_token(&mut self) -> Res<Token> {
        let peek = self.peek()?;
        while peek != Eof {
            if peek == Eol {
                self.next_char();
                return Ok(peek);
            }

            let c = self.current_char.unwrap();

            if let Num(_) = peek {
                let mut number_str = c.to_string();
                while let Some(c) = self.next_char() {
                    if c.is_digit(10) {
                        number_str.push(c);
                    }
                    else {
                        break;
                    }
                }
                return Ok(Num(number_str.parse().expect("parse to integer")))
            }

            // non-digit as here
            if let Id(_) = peek {
                let mut id = c.to_string();
                while let Some(c) = self.next_char() {
                    if id_char(c) {
                        id.push(c);
                    }
                    else {
                        break;
                    }
                }
                return Ok(Token::parse_id(id));
            }

            self.next_char();
            if let OpAss(_) = peek {
                self.next_char();
            }
            return Ok(peek);
        }

        Ok(Token::Eof)
    }
}
