#![allow(non_snake_case)]

use std::str;
use std::str::from_utf8;
use std::fmt::Debug;
use std::clone::Clone;
use std::marker::Copy;
use std::cmp::PartialEq;
use std::ops::{
    Range,
    RangeFrom,
    RangeFull,
    RangeTo
};

extern crate nom;
use self::nom::*;
use self::nom::{
    Offset,
    InputTake
};
use expression::Node;
use position_tracker::PosStr;

pub trait Nommable<'a>: 
Clone +
PartialEq +
Copy +
Debug +
Compare<Self> +
Compare<&'a str> +
Compare<&'a [u8]> +
// TODO: should this actually always be returning a byte array?
FindSubstring<&'a [u8]> +
InputIter +
InputLength +
Slice<RangeFrom<usize>> +
Slice<RangeTo<usize>> +
Slice<Range<usize>> +
Slice<RangeFull> +
AtEof +
InputTake + 
Offset
{}

impl <'a, 'b> Nommable<'a> for &'a[u8] {}
impl <'a, 'b> Nommable<'b> for PosStr<'a> {}


/// Map the contents of an IResult.
/// Rust functors plox
pub fn fmap_iresult<'a, I: Nommable<'a>, X, T, F>(res: IResult<I, X>, func: F) -> IResult<I, T>
    where F: Fn(X) -> T {
    return match res {
        Ok((i, o)) => Ok((i, func(o))),
        Err(e) => Err(e)
    };
}

pub fn fmap_node<'a, I: Nommable<'a>, X, T, F>(res: IResult<I, X>, func: F) -> IResult<I, Node<T>>
    where F: Fn(X) -> T {
    return match res {
        Ok((i, o)) => Ok((i, Node::from(func(o)))),
        Err(e) => Err(e)
    };
}

pub fn fmap_convert<'a, I: Nommable<'a>, X>(res: IResult<I, X>) -> IResult<I, Node<X>> {
    return match res {
        Ok((i, o)) => Ok((i, Node::from(o))),
        Err(e) => Err(e)
    };
}

pub fn fmap_and_full_log<'a, I: Nommable<'a>, X, T>(res: IResult<I, X>, func: fn(X) -> T, name: &str, input: I) -> IResult<I, T> {
    return match res {
        Ok((i, o)) => {
            println!("{} leftover input is {:?}. Input was: {:?}", name, i, input);
            Ok((i, func(o)))
        },
        Err(e) => {
            println!("{} error: {:?}. Input was: {:?}", name, e, input);
            Err(e) 
        }
    };
}

// TODO: Change
/// Map an IResult and log errors and incomplete values.
pub fn fmap_and_log<'a, I: Nommable<'a>, X, T>(res: IResult<I, X>, func: fn(X) -> T, name: &str, input: I) -> IResult<I, T> {
    return match res {
        Ok((i, o)) => Ok((i, func(o))),
        Err(e) => {
            println!("{} error: {:?}. Input was: {:?}", name, e, input);
            Err(e)
        }
    };
}

pub fn full_log<'a, I: Nommable<'a>, X>(res: IResult<I, X>, name: &str, input: I) -> IResult<I, X> {
    return fmap_and_full_log(res, |x| x, name, input);
}

pub fn log_err<'a, I: Nommable<'a>, X>(res: IResult<I, X>, name: &str, input: I) -> IResult<I, X> {
    return fmap_and_log(res, |x| x, name, input);
}

pub fn wrap_err<'a, I: Nommable<'a>, T>(input: I, error: ErrorKind) -> IResult<I, T> {
    return Err(Err::Error(Context::Code(input, error)));
}

pub fn output<'a, I: Nommable<'a>, T>(res: IResult<I, T>) -> T {
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

/// Alias for many0!(complete!())
macro_rules! many0c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many0!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many0c!($i, call!($f));
  );
);

/// Alias for many1!(complete!())
macro_rules! many1c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many1!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many1c!($i, call!($f));
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
                tuple!(
                    COLON,
                    NEWLINE
                )
            ),
            call!(block, $indent + 1)
        )
    );

    ($i:expr, $f: expr, $indent: expr) => (
        tuple!($i,
            terminated!(
                call!($f),
                tuple!(
                    COLON,
                    NEWLINE
                )
            ),
            call!(block, $indent + 1)
        )
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

/// A macro for consuming spaces following a submacro.
/// w_followed!(submacro!) will recognize all strings of the form:
/// "x" + " " * n
/// where "x" is recognized by the submacro.
macro_rules! w_followed (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            match tuple!($i, $submac!($($args)*), many0c!(inline_whitespace_char)) {
                Ok((remaining, (o, _))) => Ok((remaining, o)),
                Err(x) => Err(x)
            }
        }
    );

    ($i:expr, $f:expr) => (
        w_followed!($i, call!($f));
    );
);

#[inline]
pub fn inline_whitespace_char<'a, I: Nommable<'a>>(input: I) -> IResult<I, I> {
    return tag!(input, " ");
}

pub fn eof_or_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return alt!(input, eof!() | tag!("\n"));
}

pub fn between_statement(input: &[u8]) -> IResult<&[u8], Vec<Vec<&[u8]>>> {
    let n = many0c!(input,
        terminated!(many0c!(inline_whitespace_char), eof_or_line)
    );

    return n;
}

pub mod tokens {
    use super::*;
    use expression::Identifier;

    static RESERVED_WORDS: &'static [&[u8]] = &[b"if", b"else", b"elif", b"for", b"while", b"and", b"or", b"not", b"xor", b"fn", b"import", b"true", b"false", b"in", b"match", b"pass", b"continue", b"break", b"yield", b"let"];

    macro_rules! token {
        ($name:ident, $i: expr) => {
            pub fn $name<'a, T: Nommable<'a>>(input: T) -> IResult<T, T> {
                return w_followed!(input, tag!($i));
            }
        };
    }

    macro_rules! keyword {
        ($name:ident, $i: expr) => {
            pub fn $name<'a, T: Nommable<'a>>(input: T) -> IResult<T, T> 
            where <T as InputIter>::RawItem: AsChar {
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

    pub fn IDENT_CHAR<'a, T: Nommable<'a>>(input: T) -> IResult<T, T>
    where <T as InputIter>::RawItem: AsChar {
        let identifier_segment = input.position(|x| {
            let y = x.as_char();
            !(y.is_alpha() || y.is_dec_digit() || y == '_') 
        });
        return match identifier_segment {
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
        let tag_iter = RESERVED_WORDS.iter().map(|x| x.as_bytes()).map(tag_lam);
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
            Ok((i, o)) => match RESERVED_WORDS.iter().find(|x| *x == &o) {
                Some(_) => wrap_err(i, ErrorKind::Alt),
                None => Ok((i, o))
            },
            Err(x) => Err(x)
        };
        return fmap_iresult(intermediate, Identifier::from);
    }

    pub fn VALID_NUM_FOLLOW <'a, T: Nommable<'a>> (input: T) -> IResult<T, T> {
        if input.input_len() == 0 {
            return Ok((input.clone(), input.clone()));
        } else {
            return peek!(input.clone(), alt!(
                eof!() | 
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

    pub fn NEWLINE <'a, T: Nommable<'a>> (input: T) -> IResult<T, T> {
        return tag!(input, "\n");
    }

    pub fn SIGN <'a, T: Nommable<'a>> (input: T) -> IResult<T, T> {
        return recognize!(input, alt!(tag!("+") | tag!("-")));
    }

    // Keywords
    keyword!(FN, "fn");
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
    token!(TARROW, "->");

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
    token!(VBAR, "|");
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

        #[cfg(test)]
    mod tokens_test {
        use super::*;
        use self::iresult_helpers::*;

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


pub mod iresult_helpers {

    use super::*;

    pub fn check_match_and_leftover<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: T, expected_leftover: &str)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Ok((i, o)) => {
                assert_eq!(i, expected_leftover.as_bytes());
                assert_eq!(o, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {}", e, input)
            }
        }
    }

    pub fn check_data_and_leftover<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], Node<T>>, expected: T, expected_leftover: &str)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Ok((i, o)) => {
                assert_eq!(i, expected_leftover.as_bytes());
                assert_eq!(o.data, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {}", e, input)
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
                        Context::Code(_, actual_err) => assert_eq!(actual_err, expected)
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

}