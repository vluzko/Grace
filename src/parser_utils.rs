#![allow(non_snake_case)]

use std::str;
use std::str::from_utf8;
use std::fmt::Debug;
use std::clone::Clone;
use std::cmp::PartialEq;

extern crate nom;
use self::nom::*;
use self::nom::Offset;
// use self::nom::verbose_errors::Context;
use expression::Node;

use std::ops::{
    Range,
    RangeFrom,
    RangeFull,
    RangeTo
};

pub trait Nommable<'a, T>: 
Clone+
PartialEq+
Debug+
Compare<T>+
Compare<&'a str>+
Compare<&'a [u8]>+
FindSubstring<T>+
InputIter+
InputLength+
Slice<RangeFrom<usize>>+
Slice<RangeTo<usize>>+
Slice<Range<usize>>+
Slice<RangeFull>+
Offset{}

impl <'a, 'b> Nommable<'a, &'b[u8]> for &'a[u8] {}


/// Map the contents of an IResult.
/// Rust functors plox
pub fn fmap_iresult<X, T, F>(res: IResult<&[u8], X>, func: F) -> IResult<&[u8], T>
    where F: Fn(X) -> T {
    return match res {
        Ok((i, o)) => Ok((i, func(o))),
        Err(e) => Err(e)
    };
}

pub fn fmap_node<X, T, F>(res: IResult<&[u8], X>, func: F) -> IResult<&[u8], Node<T>>
    where F: Fn(X) -> T {
    return match res {
        Ok((i, o)) => Ok((i, Node::from(func(o)))),
        Err(e) => Err(e)
    };
}

pub fn fmap_convert<X>(res: IResult<&[u8], X>) -> IResult<&[u8], Node<X>> {
    return match res {
        Ok((i, o)) => Ok((i, Node::from(o))),
        Err(e) => Err(e)
    };
}

pub fn fmap_and_full_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    return match res {
        Ok((i, o)) => {
            println!("{} leftover input is {:?}. Input was: {:?}", name, from_utf8(i), from_utf8(input));
            Ok((i, func(o)))
        },
        Err(e) => {
            println!("{} error: {:?}. Input was: {:?}", name, e, from_utf8(input));
            Err(e) 
        }
    };
}

// TODO: Change
/// Map an IResult and log errors and incomplete values.
pub fn fmap_and_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    return match res {
        Ok((i, o)) => Ok((i, func(o))),
        Err(e) => {
            println!("{} error: {:?}. Input was: {:?}", name, e, from_utf8(input));
            Err(e)
        }
    };
}

pub fn full_log<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_full_log(res, |x| x, name, input);
}

pub fn log_err<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_log(res, |x| x, name, input);
}

pub fn wrap_err<T>(input: &[u8], error: ErrorKind) -> IResult<&[u8], T> {
    return Err(Err::Error(Context::Code(input, error)));
}

pub fn output<T>(res: IResult<&[u8], T>) -> T {
    return match res {
        Ok((_, o)) => o,
        Err(e) => {
            println!("Output error: {:?}.", e);
            panic!()
        }
    };
}


/// Alias for opt!(complete!())
macro_rules! optc (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    opt!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    optc!($i, call!($f));
  );
);

macro_rules! many0c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many0!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many0c!($i, call!($f));
  );
);

macro_rules! many1c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many1!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many1c!($i, call!($f));
  );
);



pub fn eof_or_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return alt!(input, eof!() | tag!("\n"));
}

pub fn between_statement(input: &[u8]) -> IResult<&[u8], Vec<Vec<&[u8]>>> {
    let n = many0c!(input,
        terminated!(many0c!(tag!(" ")), alt!(custom_eof | tag!("\n")))
    );

    return n;
}

pub fn custom_eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return eof!(input, );
}

named!(pub inline_whitespace_char<&[u8], &[u8]>,
    tag!(" ")
);

// pub fn inline_whitespace_char(input: &[u8]) -> IResult<&[u8], &[u8]> {
//     return tag!(input, " ");
// }

// pub fn inline_whitespace(input: &[u8])

named!(pub inline_whitespace<&[u8], Vec<&[u8]>>,
    many0c!(inline_whitespace_char)
);

macro_rules! ta(
  ($i:expr, $tag: expr) => ({
      match tag!($i, $tag) {
            Err(y) => panic!(),
            x => {println!("{:?}", x); x}
      }
    // match nom::bytes::streaming::tag($tag)($i) {
    //     x => {println!("{:?}", x); panic!()}
    // }
  });
);


/// Matches a keyword within a line.
/// Used for "and", "or", "xor", "in", etc.
macro_rules! inline_keyword (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      delimited!($i,
        inline_whitespace,
        $submac!($($args)*),
        preceded!(not!(valid_identifier_char), alt!(recognize!(many0c!(inline_whitespace_char)) | peek!(tag!("(")))))
    }
  );

  ($i:expr, $f:expr) => (
    inline_keyword!($i, tag!($f));
  );
);

/// Matches a keyword at the beginning of input.
/// Used for "return", etc.
macro_rules! initial_keyword (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      terminated!($i,
        $submac!($($args)*),
        preceded!(not!(valid_identifier_char), alt!(recognize!(many1!(inline_whitespace_char)) | peek!(tag!("(")))))
    }
  );

  ($i:expr, $f:expr) => (
    initial_keyword!($i, tag!($f));
  );
);

/// Check that a macro is indented correctly.
macro_rules! indented (
  ($i:expr, $submac:ident!( $($args:tt)* ), $ind:expr) => (
    preceded!($i, complete!(many_m_n!($ind, $ind, tag!(" "))), $submac!($($args)*))
  );

  ($i:expr, $f:expr, $ind: expr) => (
    indented!($i, call!($f), $ind);
  );
);

macro_rules! separated_at_least_m {
    ($i:expr, $m: expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => ({
        match separated_list_complete!($i, complete!($sep!($($args)*)), complete!($submac!($($args2)*))) {
            Ok((i, o)) => {
                if o.len() < $m {
                    wrap_err($i, ErrorKind::ManyMN)
                } else {
                    Ok((i, o))
                }
            },
            x => x
        }
    });

    ($i:expr, $m: expr, $submac:ident!( $($args:tt)* ), $g:expr) => (
        separated_at_least_m!($i, $m, $submac!($($args)*), call!($g));
    );
    ($i:expr, $m: expr, $f:expr, $submac:ident!( $($args:tt)* )) => (
        separated_at_least_m!($i, $m, call!($f), $submac!($($args)*));
    );
    ($i:expr, $m: expr, $f:expr, $g:expr) => (
        separated_at_least_m!($i, $m, call!($f), call!($g));
    );
}

macro_rules! line_and_block (
    ($i:expr, $submac: ident!($($args:tt)* ), $indent: expr) => (
        tuple!($i,
            terminated!(
                $submac!($($args)*),
                ending_colon
            ),
            call!(block, $indent + 1)
        )
    );

    ($i:expr, $f: expr, $indent: expr) => (
        tuple!($i,
            terminated!(
                call!($f),
                ending_colon
            ),
            call!(block, $indent + 1)
        )
    );
);

/// Create a rule of the form: KEYWORD SUBPARSER COLON BLOCK
/// if, elif, except, fn are all rules of this form.
macro_rules! line_then_block (
    ($i:expr, $keyword: expr, $submac: ident!($($args:tt)* ), $indent: expr) => (
        tuple!($i,
            delimited!(
                terminated!(
                    tag!($keyword),
                    not!(valid_identifier_char)
                ),
                inline_wrapped!($submac!($($args)*)),
                ending_colon
            ),
            call!(block, $indent + 1)
        )
    );

    ($i:expr, $keyword: expr, $func: expr, $indent: expr) => (
        line_then_block!($i, $keyword, call!($func), $indent)
    );
);

macro_rules! keyword_and_block (
    ($i:expr, $keyword: expr, $indent: expr) => (
        match line_and_block!($i, $keyword, $indent) {
            Ok((remaining, (_,o))) => Ok((remaining, o)),
            Err(e) => Err(e)
        }
    );
);

/// A macro for wrapping a parser in inline whitespace.
/// Similar to ws!, but doesn't allow for \n, \r, or \t.
macro_rules! inline_wrapped (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            match tuple!($i, inline_whitespace, $submac!($($args)*), inline_whitespace) {
                Ok((remaining, (_,o, _))) => {
                    Ok((remaining, o))
                },
                Err(e) => Err(e)
            }
        }
    );

    ($i:expr, $f:expr) => (
        inline_wrapped!($i, call!($f));
    );
);

/// A macro for consuming spaces following a submacro.
/// w_followed!(submacro!) will recognize all strings of the form:
/// "x" + " " * n
/// where "x" is recognized by the submacro.
macro_rules! w_followed (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            match tuple!($i, $submac!($($args)*), inline_whitespace) {
                Ok((remaining, (o, _))) => Ok((remaining, o)),
                Err(x) => Err(x)
            }
        }
    );

    ($i:expr, $f:expr) => (
        w_followed!($i, call!($f));
    );
);

/// Match any of a list of strings. Return the matched string.
pub fn match_any<'a>(input: &'a[u8], keywords: &Vec<&str>) -> IResult<&'a[u8], &'a[u8]> {
    // println!("keywords: {:?}", keywords);
    let tag_lam = |x: &[u8]| {
        let val = w_followed!(input, recognize!(complete!(tag!(x))));
        // println!("input: {:?}. val: {:?}", input, val);
        val
    };
    let tag_iter = keywords.iter().map(|x| x.as_bytes()).map(tag_lam);
    let mut ret = wrap_err(input, ErrorKind::Tag);
    for res in tag_iter {
        match res {
            Ok((i, o)) => ret = Ok((i, o)),
            _ => continue
        };
    }
    // println!("Final val: {:?}", ret);
    return ret
}

named!(pub valid_identifier_char<&[u8], &[u8]>,
    alt!(alpha | tag!("_") | digit)
);

// TODO: These should all be all caps.
named!(pub equals <&[u8], &[u8]>,
    w_followed!(tag!("="))
);

named!(pub comma <&[u8], &[u8]>,
    w_followed!(tag!(","))
);

named!(pub open_paren <&[u8], &[u8]>,
    w_followed!(tag!("("))
);

named!(pub close_paren <&[u8], &[u8]>,
    w_followed!(tag!(")"))
);

named!(pub open_brace <&[u8], &[u8]>,
    w_followed!(tag!("{"))
);

named!(pub close_brace <&[u8], &[u8]>,
    w_followed!(tag!("}"))
);

named!(pub open_bracket <&[u8], &[u8]>,
    w_followed!(tag!("["))
);

named!(pub close_bracket <&[u8], &[u8]>,
    w_followed!(tag!("]"))
);

named!(pub colon <&[u8], &[u8]>,
    w_followed!(tag!(":"))
);

named!(pub VBAR <&[u8], &[u8]>,
    w_followed!(tag!("|"))
);

pub fn NEWLINE(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return tag!(input, "\n");
}

named!(pub ending_colon <&[u8], &[u8]>,
    terminated!(
        inline_wrapped!(tag!(":")),
        NEWLINE
    )
);



named!(pub dec_digit<&[u8], &[u8]>,
    recognize!(alt!(
        tag!("0") |
        tag!("1") |
        tag!("2") |
        tag!("3") |
        tag!("4") |
        tag!("5") |
        tag!("6") |
        tag!("7") |
        tag!("8") |
        tag!("9")
    ))
);

named!(pub dec_seq<&[u8], &[u8]>,
    recognize!(many1!(dec_digit))
);

named!(pub sign<&[u8], &[u8]>,
    recognize!(alt!(tag!("+") | tag!("-")))
);

// pub fn sign<'a, T: Nommable<'a, T>> (input: T) -> IResult<T, T> {
//     return recognize!(input, alt!(tag!("+") | tag!("-")));
// }



pub mod tokens {
    use super::*;
    use expression::Identifier;

    static reserved_words: &'static [&[u8]] = &[b"if", b"else", b"elif", b"for", b"while", b"and", b"or", b"not", b"xor", b"fn", b"import", b"true", b"false", b"in", b"match", b"pass", b"continue", b"break", b"yield", b"let"];

    macro_rules! token {
        ($name:ident, $i: expr) => {
            pub fn $name(input: &[u8]) -> IResult<&[u8], &[u8]> {
                return w_followed!(input, tag!($i));
            }
        };
    }

    macro_rules! keyword {
        ($name:ident, $i: expr) => {
            pub fn $name(input: &[u8]) -> IResult<&[u8], &[u8]> {
                return w_followed!(input, terminated!(tag!($i), peek!(not!(IDENT_CHAR))));
            }
        };
    }

    /// Recognize an empty input.
    pub fn EMPTY(input: &[u8]) -> IResult<&[u8], &[u8]> {
        if input.len() == 0 {
            return Ok((input, b""));
        } else {
            return wrap_err(input, ErrorKind::NonEmpty);
        }
    }

    /// Recognize a non-empty sequence of decimal digits.
    pub fn DIGIT<'a>(input: &'a[u8]) -> IResult<&'a[u8], &'a[u8]> {
        return match input.position(|x| !(x >= 0x30 && x <= 0x39)) {
            Some(0) => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    /// Recognize a sequence of decimal digits.
    pub fn DIGIT0<'a>(input: &'a[u8]) -> IResult<&'a[u8], &'a[u8]> {
        return match input.position(|x| !(x >= 0x30 && x <= 0x39)) {
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    /// Recognize a sequence of (ASCII) alphabetic characters.
    pub fn ALPHA<'a>(input: &'a[u8]) -> IResult<&'a[u8], &'a[u8]> {
        return match input.position(|x| !((x >= 65 && x <= 90) || (x >= 97 && x <= 122))) {
            Some(0) => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    /// Recognize a sequence of alphanumeric characters.
    pub fn ALPHANUM<'a>(input: &'a[u8]) -> IResult<&'a[u8], &'a[u8]> {
        return match input.position(|x: u8| !x.is_alpha() && !x.is_dec_digit()) {
            Some(0) => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    pub fn IDENT_CHAR(input: &[u8]) -> IResult<&[u8], &[u8]> {
        return match input.position(|x: u8| !(x.is_alpha() || x.is_dec_digit() || x == 95)) {
            Some(0) => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    pub fn STRING_CHAR(input: &[u8]) -> IResult<&[u8], &[u8]>{
        return alt!(input,
            tag!("\\\"") |
            tag!("\\\\") |
            tag!("\\\n") |
            tag!("\\\r") |
            recognize!(none_of!("\n\""))
        );
    }

    /// Return true if a key
    pub fn RESERVED(input: &[u8]) -> IResult<&[u8], &[u8]> {
        let tag_lam = |x: &[u8]| recognize!(input, complete!(tag!(x)));
        let tag_iter = reserved_words.iter().map(|x| x.as_bytes()).map(tag_lam);
        let mut final_result: IResult<&[u8], &[u8]> = wrap_err(input, ErrorKind::Tag);
        for res in tag_iter {
            match res {
                Ok((i, o)) => {
                    final_result = Ok((i, o));
                    break;
                },
                _ => continue
            };
        }
        return final_result;
    }

    /// Parser to return an Identifier AST.
    pub fn IDENTIFIER(input: &[u8]) -> IResult<&[u8], Identifier> {
        let parse_result = w_followed!(input,
            recognize!(
                pair!(
                    alt!(ALPHA | tag!("_")),
                    optc!(IDENT_CHAR)
                )
            )
        );
        let intermediate = match parse_result {
            Ok((i, o)) => match reserved_words.iter().find(|x| *x == &o) {
                Some(_) => wrap_err(i, ErrorKind::Alt),
                None => Ok((i, o))
            },
            Err(x) => Err(x)
        };
        return return fmap_iresult(intermediate, Identifier::from);
    }

    pub fn VALID_NUM_FOLLOW(input: &[u8]) -> IResult<&[u8], &[u8]> {
        if input.len() == 0 {
            return Ok((input, b""));
        } else {
            return peek!(input, alt!(
                custom_eof | 
                tag!(" ") | 
                tag!("(") | 
                tag!(")") | 
                tag!(":") | 
                tag!("\n")| 
                tag!(",") | 
                tag!("]") | 
                tag!("}") |
                tag!("=>")
            ));
        }
    }

    named!(pub exponent<&[u8], (Option<&[u8]>, &[u8])>,
        preceded!(
            alt!(tag!("e") | tag!("E")),
            tuple!(
                opt!(sign),
                dec_seq
            )
        )
    );

    // Keywords
    keyword!(IF, "if");
    keyword!(ELIF, "elif");
    keyword!(ELSE, "else");
    keyword!(FOR, "for");
    keyword!(IN, "in");
    keyword!(WHILE, "while");
    keyword!(TRY, "try");
    keyword!(EXCEPT, "except");
    keyword!(FINALLY, "finally");
    keyword!(LET, "let");
    keyword!(IMPORT, "import");
    keyword!(RETURN, "return");
    keyword!(YIELD, "yield");
    keyword!(PASS, "pass");
    keyword!(BREAK, "break");
    keyword!(CONTINUE, "continue");

    keyword!(MATCH, "match");

    // Syntax
    token!(COMMA, ",");
    token!(COLON, ":");
    token!(DOT, ".");
    token!(EQUALS, "=");
    token!(OPEN_PAREN, "(");
    token!(CLOSE_PAREN, ")");
    token!(OPEN_BRACKET, "[");
    token!(CLOSE_BRACKET, "]");
    token!(OPEN_BRACE, "{");
    token!(CLOSE_BRACE, "}");
    token!(LANGLE, "<");
    token!(RANGLE, ">");
    token!(ARROW, "=>");

    // Assignments
    token!(ADDASN, "+=");
    token!(SUBASN, "-=");
    token!(MULASN, "*=");
    token!(DIVASN, "/=");
    token!(MODASN, "%=");
    token!(EXPASN, "**=");
    token!(RSHASN, ">>=");
    token!(LSHASN, "<<=");
    token!(BORASN, "|=");
    token!(BANDASN, "&=");
    token!(BXORASN, "^=");

    // Binary operators
    // Logical operators
    keyword!(AND, "and");
    keyword!(OR, "or");
    keyword!(XOR, "xor");

    // Bitwise operators
    token!(BAND, "&");
    token!(BXOR, "^");

    // Bitshift operators
    token!(LSHIFT, "<<");
    token!(RSHIFT, ">>");

    // Arithmetic operators
    token!(PLUS, "+");
    token!(MINUS, "-");
    token!(STAR, "*");
    token!(DIV, "/");
    token!(MOD, "%");
    token!(EXP, "**");

    // Comparisons
    token!(DEQUAL, "==");
    token!(NEQUAL, "!=");
    token!(LEQUAL, "<=");
    token!(GEQUAL, ">=");

    // Unary operators
    token!(NOT, "not");
    token!(TILDE, "~");
}


pub mod iresult_helpers {

    use super::*;

    pub fn check_match_and_leftover<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: T, expected_leftover: &str)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i, expected_leftover.as_bytes());
                assert_eq!(o, expected);
            },
            Result::Err(e) => {
                println!("Error: {:?}. Input was: {}", e, input);
                panic!()
            }
        }
    }

    pub fn check_data_and_leftover<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], Node<T>>, expected: T, expected_leftover: &str)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i, expected_leftover.as_bytes());
                assert_eq!(o.data, expected);
            },
            Result::Err(e) => {
                println!("Error: {:?}. Input was: {}", e, input);
                panic!()
            }
        }
    }

    pub fn check_match<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: T)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i, "".as_bytes(), "Leftover input should have been empty, was: {:?}\nResults were: {}", from_utf8(i), l_r);
                assert_eq!(o, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {}", e, input)
            }
        }
    }

    pub fn check_data<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], Node<T>>, expected: T)
    where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        return match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i, "".as_bytes(), "Leftover input should have been empty, was: {:?}\nResults were: {}", from_utf8(i), l_r);
                assert_eq!(o.data, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {}", e, input)
            }
        };
    }

    pub fn simple_check_failed<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>) {
        let res = parser(input.as_bytes());
        match res {
            Result::Err(_) => {},
            _ => panic!()
        }
    }

    pub fn unwrap_and_check_error<T>(result: IResult<&[u8], T>, expected: ErrorKind)
    where T: Debug {
        match result {
            Result::Err(e) => {
                match e {
                    Err::Error(actual_err) => match actual_err {
                        Context::Code(i, actual_err) => assert_eq!(actual_err, expected),
                        _ => panic!()
                    }
                    _ => panic!()
                }
            },
            Ok(x) => {
                println!("Should have failed, got: {:?}", x);
                panic!()
            }
        }
    }

    pub fn check_failed<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: ErrorKind)
    where T: Debug {
        let res = parser(input.as_bytes());
        unwrap_and_check_error(res, expected);
    }
}


#[cfg(test)]
mod tests {
    
    use super::*;
    use self::iresult_helpers::*;
    use expression::Identifier;

    #[cfg(test)]
    mod tokens_test {
        use super::*;
        use self::tokens::*;

        #[test]
        fn parse_digits() {
            let res = DIGIT("123".as_bytes());
            assert_eq!(res, Ok(("".as_bytes(), "123".as_bytes())));

            let res = DIGIT("123a".as_bytes());
            assert_eq!(res, Ok(("a".as_bytes(), "123".as_bytes())));

            let res = DIGIT("a".as_bytes());
            assert_eq!(res, wrap_err("a".as_bytes(), ErrorKind::Digit));
        }

        #[test]
        fn parse_inline_whitespace() {
            let input = "   ".as_bytes();
            let result = inline_whitespace(input);
            match result {
                Ok((leftover, parsed)) => {
                    assert_eq!(parsed.len(), 3);
                    assert_eq!(leftover.len(), 0);
                },
                _ => panic!()
            };
        }

        #[test]
        fn parse_identifier_raw() {
            // Basic tests
            check_match("a123", IDENTIFIER, Identifier::from("a123"));
            check_match("a_b_1_a", IDENTIFIER, Identifier::from("a_b_1_a"));

            // Check that trailers won't be matched.
            check_match_and_leftover("ident(", IDENTIFIER, Identifier::from("ident"), "("); 
            check_match_and_leftover("func()", IDENTIFIER, Identifier::from("func"), "()");

            // Check that whitespace is consumed.
            check_match_and_leftover("abc     ", IDENTIFIER, Identifier::from("abc"), "");

            // Check that identifiers can't start with digits.
            check_failed("123", IDENTIFIER, ErrorKind::Alt);    

            // Check that reserved words aren't matched.
            check_failed("true", IDENTIFIER, ErrorKind::Alt);
        }
    }

}