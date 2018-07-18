use std::fmt;

/// `SourceRef`((`line_start`, `character_start`), (`line_end`, `character_end`))
/// inclusive `character_start`, exclusive `character_end`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceRef(pub (usize, usize), pub (usize, usize));

impl SourceRef {
    pub fn with_char_end(&self, end: (usize, usize)) -> SourceRef {
        SourceRef(self.0, end)
    }

    pub fn up_to(&self, other: SourceRef) -> SourceRef {
        self.with_char_end(other.0)
    }

    pub fn up_to_end_of(&self, other: SourceRef) -> SourceRef {
        self.with_char_end(other.1)
    }

    pub fn up_to_next_line(self) -> SourceRef {
        SourceRef(self.0, ((self.0).0 + 1, 1))
    }

    pub fn contains(&self, SourceRef((ofl, ofc), (otl, otc)): SourceRef) -> bool {
        let SourceRef((fl, fc), (tl, tc)) = *self;
        ofl >= fl && otl <= tl && ofc >= fc && otc <= tc
    }
}

impl fmt::Debug for SourceRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SourceRef((sl, sc), (el, ec)) = *self;
        if sl == el && sc + 1 == ec {
            write!(f, "{}:{}", sl, sc)
        } else if sl == el {
            write!(f, "{}:[{},{})", sl, sc, ec)
        } else {
            write!(f, "[{}:{},{}:{})", sl, sc, el, ec)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Stage {
    Lexer,
    Parser,
    Interpreter,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BadderError {
    pub stage: Stage,
    pub description: String,
    pub src: SourceRef,
}

impl fmt::Debug for BadderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}: {}", self.src, self.stage, self.description)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PartialBadderError(SourceRef);

impl BadderError {
    pub fn at(src_ref: SourceRef) -> PartialBadderError {
        PartialBadderError(src_ref)
    }
}

impl PartialBadderError {
    pub fn describe<S: Into<String>>(self, stage: Stage, desc: S) -> BadderError {
        BadderError {
            stage,
            description: desc.into(),
            src: self.0,
        }
    }
}

pub type Res<T> = Result<T, BadderError>;
