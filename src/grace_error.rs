use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;

extern crate nom;
use self::nom::ErrorKind;


/// A Grace error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraceError {
    ParserError{msg: String, nom_error: ErrorKind},
    TypeError{msg: String}
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