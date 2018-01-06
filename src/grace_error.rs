use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum GraceError {
    InvalidCharacter(CharError),
    InvalidRowLength(SizeError),
    GenericError
}

impl fmt::Display for GraceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GraceError::InvalidCharacter(ref c) => write!(f, "InvalidCharacter: {}: expected `{}`, got `{}`",
                                                        c.position, c.expected, c.got),
            GraceError::InvalidRowLength(ref c) => write!(f, "InvalidRowLength: {}: expected `{}` element{}, got `{}`",
                                                        c.position, c.nb_elements_expected,
                                                        if c.nb_elements_expected > 1 { "s" } else { "" }, c.nb_elements_got),
            GraceError::GenericError            => write!(f, "GenericError")
        }
    }
}

impl PartialEq for GraceError {
    fn eq(&self, other: &GraceError) -> bool {
        match (self, other) {
            (&GraceError::InvalidCharacter(ref c), &GraceError::InvalidCharacter(ref o)) => c == o,
            (&GraceError::InvalidRowLength(ref c), &GraceError::InvalidRowLength(ref o)) => c == o,
            (&GraceError::GenericError, &GraceError::GenericError)                       => true,
            _ => false,
        }
    }

    fn ne(&self, other: &GraceError) -> bool {
        self.eq(other) == false
    }
}

impl Error for GraceError {
    fn description(&self) -> &str {
        match *self {
            GraceError::InvalidCharacter(_) => "invalid character",
            GraceError::InvalidRowLength(_) => "invalid row length",
            GraceError::GenericError        => "generic parsing error"
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position {
            line: line,
            column: column,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CharError {
    pub expected: char,
    pub got: char,
    pub position: Position,
}

impl CharError {
    pub fn new(expected: char, got: char, pos: &Position) -> CharError {
        CharError {
            expected: expected,
            got: got,
            position: pos.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SizeError {
    pub nb_elements_expected: usize,
    pub nb_elements_got: usize,
    pub position: Position,
}

impl SizeError {
    pub fn new(nb_elements_expected: usize, nb_elements_got: usize,
               pos: &Position) -> SizeError {
        SizeError {
            nb_elements_expected: nb_elements_expected,
            nb_elements_got: nb_elements_got,
            position: pos.clone(),
        }
    }
}
