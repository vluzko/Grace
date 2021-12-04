//! Error messages.
use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;
use std::ops::Add;

extern crate nom;
use self::nom::ErrorKind;
use general_utils;

/// A Grace error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraceError {
    /// A parsing error
    ParserError { msg: String, nom_error: ErrorKind },
    /// A typing error
    TypeError { msg: String },
    /// A set of errors
    MultiError { errors: Vec<GraceError> },
    /// An error in the underlying compiler.
    /// Usually this means that code that should be impossible to run, ran (e.g. type checking the same thing twice returns different values)
    CompilerError { msg: String },
}

impl Add for GraceError {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        return match self {
            GraceError::MultiError { errors: errors1 } => match other {
                GraceError::MultiError { errors: errors2 } => GraceError::MultiError {
                    errors: general_utils::join(errors1, errors2)
                },
                x => GraceError::MultiError {
                    errors: errors1.into_iter().chain(vec![x].into_iter()).collect(),
                },
            },
            _ => GraceError::MultiError {
                errors: vec![self, other],
            },
        };
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
