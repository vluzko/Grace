use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;

extern crate nom;
use self::nom::ErrorKind;

use position_tracker::PosStr;

/// A Grace error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraceError<'a> {
    ParserError{msg: String, nom_error: (PosStr<'a>, ErrorKind)},
    TypeError{msg: String}
}

impl <'a> fmt::Display for GraceError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        panic!()
    }
}

impl <'a> Error for GraceError<'a> {
    fn description(&self) -> &str {
        panic!()
    }
}