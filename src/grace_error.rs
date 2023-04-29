//! Error messages.
use std::cmp::PartialEq;
use std::error::Error;
use std::fmt;
use std::ops::Add;

extern crate nom;
use self::nom::ErrorKind;

use expression::Node;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GraceError {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub underlying: ErrorDetails,
}

/// A Grace error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorDetails {
    /// A parsing error
    ParserError { msg: String, nom_error: ErrorKind },
    /// A scoping error
    ScopeError { msg: String },
    /// A typing error
    TypeError { msg: String },
    /// A set of errors
    MultiError { errors: Vec<GraceError> },
    /// An error in the underlying compiler.
    /// Usually this means that code that should be impossible to run, ran (e.g. type checking the same thing twice returns different values)
    CompilerError { msg: String },
}

impl GraceError {
    /// Create a parser error from a message and a Nom error
    pub fn parser_error(msg: String, nom_error: ErrorKind) -> GraceError {
        return GraceError {
            file: String::new(),
            line: 0,
            column: 0,
            end_line: 0,
            end_column: 0,
            underlying: ErrorDetails::ParserError {
                msg: msg,
                nom_error: nom_error,
            },
        };
    }

    /// Create a scoping error
    pub fn scoping_error(msg: String) -> GraceError {
        GraceError {
            file: String::new(),
            line: 0,
            column: 0,
            end_line: 0,
            end_column: 0,
            underlying: ErrorDetails::ScopeError { msg: msg },
        }
    }

    /// Create a type error from a message
    pub fn type_error(msg: String) -> GraceError {
        GraceError {
            file: String::new(),
            line: 0,
            column: 0,
            end_line: 0,
            end_column: 0,
            underlying: ErrorDetails::TypeError { msg: msg },
        }
    }

    /// Create a compiler error from a message.
    pub fn compiler_error(msg: String) -> GraceError {
        return GraceError {
            file: String::new(),
            line: 0,
            column: 0,
            end_line: 0,
            end_column: 0,
            underlying: ErrorDetails::CompilerError { msg: msg },
        };
    }

    pub fn multi_error(errors: Vec<GraceError>) -> GraceError {
        return GraceError {
            file: String::new(),
            line: 0,
            column: 0,
            end_line: 0,
            end_column: 0,
            underlying: ErrorDetails::MultiError { errors: errors },
        };
    }

    pub fn update_line_col(
        &self,
        line: u32,
        column: u32,
        end_line: u32,
        end_column: u32,
    ) -> GraceError {
        return GraceError {
            file: self.file.clone(),
            line: line,
            column: column,
            end_line: end_line,
            end_column: end_column,
            underlying: self.underlying.clone(),
        };
    }

    pub fn annotate_file(&self, file: String) -> GraceError {
        return GraceError {
            file: file,
            line: self.line,
            column: self.column,
            end_line: self.end_line,
            end_column: self.end_column,
            underlying: self.underlying.clone(),
        };
    }
}

impl Add for GraceError {
    type Output = Self;
    fn add(self, _other: Self) -> Self {
        panic!()
        // return match self {
        //     GraceError::MultiError { errors: errors1 } => match other {
        //         GraceError::MultiError { errors: errors2 } => GraceError::MultiError {
        //             errors: general_utils::join(errors1, errors2)
        //         },
        //         x => GraceError::MultiError {
        //             errors: errors1.into_iter().chain(vec![x].into_iter()).collect(),
        //         },
        //     },
        //     _ => GraceError::MultiError {
        //         errors: vec![self, other],
        //     },
        // };
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

impl<T> Node<T> {
    pub fn annotate_error(&self, error: GraceError) -> GraceError {
        return error.update_line_col(self.start_line, self.start_col, self.end_line, self.end_col);
    }
}
