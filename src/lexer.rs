use super::Int;
use crate::common::*;
use std::{borrow::Cow, fmt, iter::Peekable, str::Chars};
use string_cache::DefaultAtom as Atom;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Num(Int),
    Id(Atom),

    // Operators
    Pls,
    Sub,
    Mul,
    Div,
    OpnBrace,
    ClsBrace,
    Ass,
    OpAss(Box<Token>),
    Gt,
    GtEq,
    Lt,
    LtEq,
    Mod,
    OpnSqr,
    ClsSqr,
    Square,

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
    Indent(usize),
    Loop,
    While,
    For,
    In,
    Break,
    Continue,
    Fun,
    Return,
    Comma,
    Dot,
    Seq,
}

use crate::lexer::Token::*;

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Num(x) => write!(f, "{}", x),
            Pls => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            OpnBrace => write!(f, "("),
            ClsBrace => write!(f, ")"),
            Ass => write!(f, "="),
            Gt => write!(f, ">"),
            GtEq => write!(f, ">="),
            Lt => write!(f, "<"),
            LtEq => write!(f, "<="),
            Mod => write!(f, "%"),
            OpAss(ref op) => write!(f, "{:?}=", *op),
            OpnSqr => write!(f, "["),
            ClsSqr => write!(f, "]"),
            Square => write!(f, "[]"),
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
            Loop => write!(f, "loop"),
            While => write!(f, "while"),
            For => write!(f, "for"),
            In => write!(f, "in"),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
            Fun => write!(f, "fun"),
            Return => write!(f, "return"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Seq => write!(f, "seq"),
            Indent(i) => write!(f, "'    '*{}", i),
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
            '>' => Some(Gt),
            '<' => Some(Lt),
            '%' => Some(Mod),
            ',' => Some(Comma),
            '.' => Some(Dot),
            '[' => Some(OpnSqr),
            ']' => Some(ClsSqr),
            _ => None,
        }
    }

    pub fn parse_id<A: Into<Atom>>(id: A) -> Token {
        let id = id.into();
        match id.as_ref() {
            "var" => Var,
            "if" => If,
            "else" => Else,
            "not" => Not,
            "is" => Is,
            "def" => Def,
            "and" => And,
            "or" => Or,
            "loop" => Loop,
            "while" => While,
            "for" => For,
            "in" => In,
            "break" => Break,
            "continue" => Continue,
            "fun" => Fun,
            "return" => Return,
            "seq" => Seq,
            _ => Id(id),
        }
    }

    fn is_valid_for_op_ass(&self) -> bool {
        match *self {
            Pls | Sub | Mul | Div => true,
            _ => false,
        }
    }

    pub fn matches(&self, token: &Token) -> bool {
        match (self, token) {
            (&Num(_), &Num(_)) | (&Id(_), &Id(_)) | (&Indent(_), &Indent(_)) => true,
            (me, other) => me == other,
        }
    }

    pub fn long_debug(&self) -> Cow<'static, str> {
        match *self {
            Num(x) => format!("number `{}`", x).into(),
            Id(ref id) => format!("id `{}`", id).into(),
            Pls | Sub | Mul | Div | OpnBrace | ClsBrace | Ass | OpAss(_) | Gt | Lt | GtEq
            | LtEq => format!("operator `{:?}`", self).into(),
            Indent(_) => format!("indent {:?}", self).into(),
            Eol => "end-of-line".into(),
            Eof => "end-of-file".into(),
            Comma => "`,`".into(),
            Dot => "`.`".into(),
            _ => format!("keyword `{:?}`", self).into(),
        }
    }

    pub fn id_str(&self) -> Option<&str> {
        if let Token::Id(ref inner) = *self {
            Some(inner)
        } else {
            None
        }
    }

    pub fn is_binary_op(&self) -> bool {
        match *self {
            Pls | Sub | Mul | Div | Ass | Gt | GtEq | Lt | LtEq | Mod | OpAss(_) | Is | And
            | Or | In | Dot => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    current_char: Option<char>,
    newline: bool,
    line_num: usize,
    char_num: usize,
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
            current_char: first,
            newline: true,
            line_num: 1,
            char_num: 1,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.newline = self.current_char == Some('\n');
        self.current_char = self.chars.next();
        if self.newline {
            self.line_num += 1;
            self.char_num = 1;
        } else {
            self.char_num += 1;
        }

        self.current_char
    }

    fn cursor(&self) -> SourceRef {
        SourceRef(
            (self.line_num, self.char_num),
            (self.line_num, self.char_num + 1),
        )
    }

    /// Attempts to return next token without advancing, has limitations
    /// Nums & Ids with only contain their first character
    /// Idents are assumed if the current character is a space on a new line
    pub fn peek(&mut self) -> Res<Token> {
        while let Some(c) = self.current_char {
            if self.newline && c == ' ' {
                // start of new line, only valid if indent
                return Ok(Indent(0));
            }

            if junkspace(c) {
                self.next_char();
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
                return Ok(Id(c.to_string().into()));
            }

            if let Some(token) = Token::parse(c) {
                if token == Gt {
                    if let Some(&'=') = self.chars.peek() {
                        return Ok(GtEq);
                    }
                }
                if token == Lt {
                    if let Some(&'=') = self.chars.peek() {
                        return Ok(LtEq);
                    }
                }
                if token.is_valid_for_op_ass() {
                    if let Some(&'=') = self.chars.peek() {
                        return Ok(OpAss(token.into()));
                    }
                }
                if token == OpnSqr {
                    if let Some(&']') = self.chars.peek() {
                        return Ok(Square);
                    }
                }
                return Ok(token);
            }

            return Err(BadderError::at(self.cursor())
                .describe(Stage::Lexer, format!("Unexpected char: `{}`", c)));
        }

        Ok(Eof)
    }

    pub fn next_token(&mut self) -> Res<(Token, SourceRef)> {
        let peek = self.peek()?;
        let src_ref = self.cursor();

        if peek == Eof {
            return Ok((peek, src_ref));
        }

        if peek == Eol {
            self.next_char();
            return Ok((peek, src_ref));
        }

        if let Indent(_) = peek {
            let mut spaces = 1;
            while let Some(' ') = self.next_char() {
                spaces += 1;
            }
            if spaces % 4 != 0 {
                // could be an indent error, or could be a junkspace line
                while let Some(c) = self.current_char {
                    if c == '\n' {
                        return self.next_token();
                    }
                    if c == '#' {
                        while let Some(c) = self.next_char() {
                            if c == '\n' {
                                return self.next_token();
                            }
                        }
                    }
                    if !junkspace(c) {
                        break;
                    }
                    self.next_char();
                }

                let hint = match spaces % 4 {
                    1 => ", try removing 1 space.",
                    3 => ", try removing 3 spaces.",
                    _ => ", try removing 2 spaces.",
                };

                return Err(BadderError::at(src_ref.up_to(self.cursor())).describe(
                    Stage::Lexer,
                    format!("Invalid indent must be multiple of 4 spaces{}", hint),
                ));
            }
            return Ok((Indent(spaces / 4), src_ref.up_to(self.cursor())));
        }

        let c = self.current_char.unwrap();

        if let Num(_) = peek {
            let mut number_str = c.to_string();
            while let Some(c) = self.next_char() {
                if c.is_digit(10) {
                    number_str.push(c);
                } else {
                    break;
                }
            }
            let src_ref = src_ref.up_to(self.cursor());
            return match number_str.parse() {
                Ok(n) => Ok((Num(n), src_ref)),
                Err(e) => Err(BadderError::at(src_ref)
                    .describe(Stage::Lexer, format!("could not parse number: {}", e))),
            };
        }

        // non-digit as here
        if let Id(_) = peek {
            let mut id = c.to_string();
            while let Some(c) = self.next_char() {
                if id_char(c) {
                    id.push(c);
                } else {
                    break;
                }
            }
            return Ok((Token::parse_id(id), src_ref.up_to(self.cursor())));
        }

        self.next_char();
        if let OpAss(_) = peek {
            self.next_char();
        }
        if peek == GtEq || peek == LtEq || peek == Square {
            self.next_char();
        }

        Ok((peek, src_ref.up_to(self.cursor())))
    }
}

#[cfg(test)]
mod lexer_test {
    use super::*;

    #[test]
    fn next_token_src_ref() {
        let mut lexer = Lexer::new("        seq some_id[] = 1345, 2");

        for (exp_token, exp_src_ref) in vec![
            (Indent(2), SourceRef((1, 1), (1, 9))),
            (Seq, SourceRef((1, 9), (1, 12))),
            (Id("some_id".into()), SourceRef((1, 13), (1, 20))),
            (Square, SourceRef((1, 20), (1, 22))),
            (Ass, SourceRef((1, 23), (1, 24))),
            (Num(1345), SourceRef((1, 25), (1, 29))),
            (Comma, SourceRef((1, 29), (1, 30))),
            (Num(2), SourceRef((1, 31), (1, 32))),
            (Eof, SourceRef((1, 32), (1, 33))),
        ] {
            let (token, src_ref) = lexer.next_token().unwrap();
            println!("Token `{:?}`", token);
            assert_eq!(token, exp_token);
            assert_eq!(src_ref, exp_src_ref);
        }
    }
}
