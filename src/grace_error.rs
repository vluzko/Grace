//! Error messages.
use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;
use std::ops::Add;

extern crate nom;
use self::nom::ErrorKind;

/// A Grace error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraceError {
    ParserError { msg: String, nom_error: ErrorKind },
    TypeError { msg: String },
    MultiError {errors: Vec<GraceError>}
}

impl Add for GraceError {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let errors = vec!(self, other);
        return Self::MultiError {errors: errors};
    }
}

impl fmt::Display for GraceError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        panic!()
    }
}

impl Error for GraceError {
    fn description(&self) -> &str {
        panic!()
    }
}
