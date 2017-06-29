use std::fmt;

/// `SourceRef`(`line`, `character_start`, `character_end`)
/// inclusive `character_start`, exclusive `character_end`
#[derive(Clone, Copy, PartialEq, Hash)]
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
}

impl fmt::Debug for SourceRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SourceRef((sl, sc), (el, ec)) = *self;
        if sl == el && sc+1 == ec { write!(f, "{}:{}", sl, sc) }
        else if sl == el { write!(f, "{}:[{},{})", sl, sc, ec) }
        else { write!(f, "[{}:{},{}:{})", sl, sc, el, ec) }
    }
}

pub struct BadderError {
    pub description: String,
    pub src: SourceRef,
}

impl fmt::Debug for BadderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {}", self.src, self.description)
    }
}

pub struct PartialBadderError(SourceRef);

impl BadderError {
    pub fn at(src_ref: SourceRef) -> PartialBadderError {
        PartialBadderError(src_ref)
    }
}

impl PartialBadderError {
    pub fn describe<S: Into<String>>(self, desc: S) -> BadderError {
        BadderError {
            description: desc.into(),
            src: self.0
        }
    }
}

pub type Res<T> = Result<T, BadderError>;
