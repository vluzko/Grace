#![allow(non_snake_case)]

use std::str;
use std::fmt::Debug;
use std::clone::Clone;
use std::cmp::PartialEq;


extern crate nom;
// use self::nom;
use self::nom::*;
use expression::Node;
use position_tracker::PosStr;

use self::tokens::*;
use self::iresult_helpers::*;

type IO<'a> = IResult<PosStr<'a>, PosStr<'a>>;
type Res<'a, T> = IResult<PosStr<'a>, T>;

/// A macro for calling methods.
macro_rules! m (
    ($i:expr, $self_:ident.$method:ident) => (
        {
            let res = $self_.$method($i);
            res
        }
    );
    ($i:expr, $self_:ident.$method:ident, $($args:expr),* ) => (
        {
            let res = $self_.$method($i, $($args),*);
            res
        }
    );
);

/// Take until the given parser matches.
macro_rules! take_until_parse (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        // let f = |x| match $submac(x) {
        //     Ok(_) => true,
        //     _ => false
        // };
        opt!($i, complete!($submac!($($args)*)))
    );
);

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

fn split_at_position_inclusive<P, T>(sequence: &T, predicate: P) -> IResult<T, T, u32>
  where
    T: InputLength + InputIter + InputTake + AtEof + Clone,
    P: Fn(<T as InputIter>::RawItem) -> bool,
  {
    match sequence.position(predicate) {
      Some(n) => Ok(sequence.take_split(n+1)),
      None => {
        if sequence.at_eof() {
          Ok(sequence.take_split(sequence.input_len()))
        } else {
          Err(Err::Incomplete(Needed::Size(1)))
        }
      }
    }
  }


macro_rules! take_till_inclusive (
    ($input:expr, $submac:ident!( $($args:tt)* )) => (
        {
            // use nom::InputTakeAtPosition;
            let input = $input;
            match split_at_position_inclusive(&input, |c| $submac!(c, $($args)*)) {
                Err(nom::Err::Incomplete(_)) => Ok(input.take_split(input.input_len())),
                x => x
            }
        }
    );
    ($input:expr, $f:expr) => (
        take_till_inclusive!($input, call!($f));
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
    ($i:expr, $self_:ident, $submac: ident!($($args:tt)* ), $indent: expr) => (
        tuple!($i,
            terminated!(
                $submac!($($args)*),
                tuple!(
                    COLON,
                    NEWLINE
                )
            ),
            m!($self_.block, $indent + 1)
        )
    );

    ($i:expr, $self_:ident, $f: expr, $indent: expr) => (
        tuple!($i,
            terminated!(
                call!($f),
                tuple!(
                    COLON,
                    NEWLINE
                )
            ),
            m!($self_.block, $indent + 1)
        )
    );
);

macro_rules! keyword_and_block (
    ($i:expr, $self_:ident, $keyword: expr, $indent: expr) => (
        match line_and_block!($i, $self_, $keyword, $indent) {
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
pub fn inline_whitespace_char<'a>(input: PosStr<'a>) -> IO<'a> {
    return tag!(input, " ");
}

pub fn eof_or_line<'a>(input: PosStr<'a>) -> IO<'a> {
    return alt!(input, eof!() | NEWLINE | EMPTY);
}

pub fn single_line_comment<'a>(input: PosStr<'a>) -> IO<'a> {
    let f = |x: u8| {
        return if x == b'\n' {
            true
        } else {
            false
        }
    };
    return recognize!(input,
        preceded!(
            tag!("//"),
            take_till_inclusive!(f)
        )
    );
}

pub fn between_statement<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Vec<Vec<PosStr<'a>>>> {
    let n = many0c!(input,
        terminated!(many0c!(inline_whitespace_char), eof_or_line)
    );

    return n;
}

pub mod tokens {
    use super::*;
    use expression::Identifier;

    static RESERVED_WORDS: &'static [&[u8]] = &[b"if", b"else", b"elif", b"for", b"while", b"and", b"or", b"not", b"xor", b"fn", b"import", b"true", b"false", b"in", b"match", b"pass", b"continue", b"break", b"yield", b"let", b"trait", b"impl", b"self"];

    macro_rules! token {
        ($name:ident, $i: expr) => {
            pub fn $name<'a>(input: PosStr<'a>) -> IO<'a> {
                return w_followed!(input, tag!($i));
            }
        };
    }

    macro_rules! keyword {
        ($name:ident, $i: expr) => {
            pub fn $name<'a>(input: PosStr<'a>) -> IO<'a> {
                return w_followed!(input, terminated!(tag!($i), peek!(not!(IDENT_CHAR))));
            }
        };
    }

    /// Recognize an empty input.
    pub fn EMPTY<'a>(input: PosStr<'a>) -> IO<'a> {
        if input.input_len() == 0 {
            return Ok((input, PosStr::empty()));
        } else {
            return wrap_err(input, ErrorKind::NonEmpty);
        }
    }

    pub fn NUM_START<'a>(input: PosStr<'a>) -> IO<'a> {
        return match input.slice.len() {
            0 => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            _ => {
                let x = input.slice[0];
                match (x >= 0x30 && x <= 0x39) || x == 0x2e {
                    true => Ok(input.take_split(1)),
                    false => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit)))
                }
            }
        };
    }

    /// Recognize a non-empty sequence of decimal digits.
    pub fn DIGIT<'a>(input: PosStr<'a>) -> IO<'a> {
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
    pub fn DIGIT0<'a>(input: PosStr<'a>) -> IO<'a> {
        return match input.position(|x| !(x >= 0x30 && x <= 0x39)) {
            Some(n) => Ok(input.take_split(n)),
            None => Ok(input.take_split(input.input_len()))
        };
    }

    /// Recognize a sequence of (ASCII) alphabetic characters.
    pub fn ALPHA<'a>(input: PosStr<'a>) -> IO<'a> {
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
    pub fn ALPHANUM<'a>(input: PosStr<'a>) -> IO<'a> {
        return match input.position(|x: u8| !x.is_alpha() && !x.is_dec_digit()) {
            Some(0) => Err(Err::Error(Context::Code(input.clone(), ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input.clone(), ErrorKind::Digit),
                n => Ok(input.take_split(n))
            }
        };
    }

    /// Recognize a sequence of valid internal identifier characters.
    pub fn IDENT_CHAR<'a>(input: PosStr<'a>) -> IO<'a> {
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

    /// Recognize a character in a string, or an escape sequence.
    pub fn STRING_CHAR<'a>(input: PosStr<'a>) -> IO<'a>{
        return alt!(input,
            tag!("\\\"") |
            tag!("\\\'") |
            tag!("\\\\") |
            tag!("\\n") |
            tag!("\\\r") |
            recognize!(none_of!("\r\n\"\\"))
        );
    }

    /// Return true if a key
    pub fn RESERVED<'a>(input: PosStr<'a>) -> IO<'a> {
        let tag_lam = |x| recognize!(input, complete!(tag!(PosStr::new(x))));
        let tag_iter = RESERVED_WORDS.iter().map(|x| x.as_bytes()).map(tag_lam);
        let mut final_result: IO<'a> = wrap_err(input, ErrorKind::Tag);
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
    pub fn IDENTIFIER<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Identifier> {
        let parse_result = w_followed!(input,
            recognize!(
                pair!(
                    alt!(ALPHA | tag!("_")),
                    optc!(IDENT_CHAR)
                )
            )
        );
        let intermediate = match parse_result {
            Ok((i, o)) => match RESERVED_WORDS.iter().find(|x| *x == &(o.slice)) {
                Some(_) => wrap_err(i, ErrorKind::Alt),
                None => Ok((i, o))
            },
            Err(x) => Err(x)
        };
        return fmap_iresult(intermediate, Identifier::from);
    }

    pub fn VALID_NUM_FOLLOW <'a> (input: PosStr<'a>) -> IO<'a> {
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
                tag!("=>") |
                tag!("+") |
                tag!("-") |
                tag!("*") |
                tag!("/") |
                tag!("%") |
                tag!("&") |
                tag!("|") |
                tag!("^") |
                tag!("!") |
                tag!("=") |
                tag!("<") |
                tag!(">")
            ));
        }
    }

    pub fn NEWLINE <'a> (input: PosStr<'a>) -> IO<'a> {
        return alt_complete!(input, tag!("\n") | single_line_comment);
    }

    pub fn SIGN <'a> (input: PosStr<'a>) -> IO<'a> {
        return recognize!(input, alt!(tag!("+") | tag!("-")));
    }

    /// Parser for the negative unary operator. Checks that it's not immediately followed by a digit.
    pub fn NEG<'a>(input: PosStr<'a>) -> IO<'a> {
        return w_followed!(input, terminated!(tag!("-"), peek!(not!(NUM_START))));
    }

    // Keywords
    keyword!(TRAIT, "trait");
    keyword!(IMPL, "impl");
    keyword!(FN, "fn");
    keyword!(STRUCT, "struct");
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
    keyword!(AS, "as");
    keyword!(RETURN, "return");
    keyword!(YIELD, "yield");
    keyword!(PASS, "pass");
    keyword!(BREAK, "break");
    keyword!(CONTINUE, "continue");
    keyword!(MATCH, "match");
    keyword!(SELF, "self");

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
    token!(UNDERSCORE, "_");

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

        #[test]
        fn parse_digits() {
            check_match("123", DIGIT, PosStr::from("123"));
            check_match_and_leftover("123a", DIGIT, PosStr::from("123"), "a");
            simple_check_failed("a", DIGIT);
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

    pub fn chain<'a, X, F, T>(res: Res<'a, X>, parser: F) -> Res<'a, (X, T)>
        where F: Fn(PosStr<'a>) -> Res<'a, T> {
            return match res {
                Ok((i, o)) => match parser(i) {
                    Ok((i_final, parser_o)) => Ok((i_final, (o, parser_o))),
                    Err(e) => Err(e)
                },
                Err(e) => Err(e)
            };
    }

    /// Map the contents of an IResult.
    /// Rust functors plox
    pub fn fmap_iresult<'a, X, T, F>(res: Res<'a, X>, func: F) -> Res<'a, T>
        where F: Fn(X) -> T {
        return match res {
            Ok((i, o)) => Ok((i, func(o))),
            Err(e) => Err(e)
        };
    }

    pub fn fmap_update<'a, X, U, T, F>(res: Res<'a, (X, U)>, func: F) -> Res<'a, (T, U)>
        where F: Fn(X) -> T {
        return match res {
            Ok((i, (o, u))) => Ok((i, (func(o), u))),
            Err(e) => Err(e)
        };
    }

    /// Map the contents and wrap a Node around it.
    pub fn fmap_node<'a, X, T, F>(res: Res<'a, X>, func: F) -> Res<'a, Node<T>>
        where F: Fn(X) -> T {
        return match res {
            Ok((i, o)) => Ok((i, Node::from(func(o)))),
            Err(e) => Err(e)
        };
    }

    pub fn fmap_pass<'a, X, U, T, F>(res: Res<'a, (X, U)>, func: F) -> Res<'a, (Node<T>, U)>
        where F: Fn(X) -> T {
        return match res {
            Ok((i, o)) => Ok((i, (Node::from(func(o.0)), o.1))),
            Err(e) => Err(e)
        };
    }

    pub fn fmap_nodeu<'a, X, U, T, F>(res: Res<'a, X>, func: F) -> Res<'a, (Node<T>, U)>
        where F: Fn(X) -> (T, U) {
        return match res {
            Ok((i, o)) => {
                let (v, u) = func(o);
                Ok((i, (Node::from(v), u)))
            },
            Err(e) => Err(e)
        };
    }

    pub fn fmap_convert<'a, X>(res: Res<'a, X>) -> Res<'a, Node<X>> {
        return match res {
            Ok((i, o)) => Ok((i, Node::from(o))),
            Err(e) => Err(e)
        };
    }

    pub fn fmap_and_full_log<'a, X, T>(res: Res<'a, X>, func: fn(X) -> T, name: &str, input: PosStr<'a>) -> Res<'a, T> {
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

    /// Map an IResult and log errors and incomplete values.
    pub fn fmap_and_log<'a, X, T>(res: Res<'a, X>, func: fn(X) -> T, name: &str, input: PosStr<'a>) -> Res<'a, T> {
        return match res {
            Ok((i, o)) => Ok((i, func(o))),
            Err(e) => {
                println!("{} error: {:?}. Input was: {:?}", name, e, input);
                Err(e)
            }
        };
    }

    pub fn full_log<'a, X>(res: Res<'a, X>, name: &str, input: PosStr<'a>) -> Res<'a, X> {
        return fmap_and_full_log(res, |x| x, name, input);
    }

    pub fn log_err<'a, X>(res: Res<'a, X>, name: &str, input: PosStr<'a>) -> Res<'a, X> {
        return fmap_and_log(res, |x| x, name, input);
    }

    pub fn wrap_err<'a, T>(input: PosStr<'a>, error: ErrorKind) -> Res<'a, T> {
        return Err(Err::Error(Context::Code(input, error)));
    }

    pub fn output<'a, T>(res: Res<'a, T>) -> T {
        return match res {
            Ok((_, o)) => o,
            Err(e) => {
                println!("Output error: {:?}.", e);
                panic!()
            }
        };
    }

    pub fn check_match_and_leftover<'a, T>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, T>, expected: T, expected_leftover: &'a str)
        where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                assert_eq!(i.slice, expected_leftover.as_bytes());
                assert_eq!(o, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    pub fn check_data_and_leftover<'a, T, U>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, (Node<T>, U)>, expected: T, expected_leftover: &str)
        where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                assert_eq!(i.slice, expected_leftover.as_bytes());
                assert_eq!(o.0.data, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    /// Check that the parser applied to the input returns the expect result.
    pub fn check_match<'a, T>(input: &'a str, parser:  impl Fn(PosStr<'a>) -> Res<'a, T>, expected: T)
        where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}", i, l_r);
                assert_eq!(o, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    pub fn check_match_no_update<'a, T, U>(input: &'a str, parser:  impl Fn(PosStr<'a>) -> Res<'a, (T, U)>, expected: T)
        where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.0);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}", i, l_r);
                assert_eq!(o.0, expected);
            },
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    /// Check just the data of the result of a parser that includes an update. Skips the containing node and the update.
    pub fn check_data<'a, T, U>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, (Node<T>, U)>, expected: T)
    where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        return match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.0.data);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}\nInput was: {}", i, l_r, input);
                assert_eq!(o.0.data, expected, "Results were: {}\nInput was: {}", l_r, input);
            },
            Result::Err(e) => {
                panic!("Error: {:?}.\nInput was: {}", e, input)
            }
        };
    }

    /// Check just the data of the result of a parser. Skips the containing node.
    pub fn check_data_no_update<'a, T>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, Node<T>>, expected: T)
    where T: Debug + PartialEq + Eq {
        let res = parser(PosStr::from(input));
        return match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.data);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}\nInput was: {}", i, l_r, input);
                assert_eq!(o.data, expected, "Results were: {}\nInput was: {}", l_r, input);
            },
            Result::Err(e) => {
                panic!("Error: {:?}.\nInput was: {}", e, input)
            }
        };
    }

    pub fn simple_check_failed<'a, T>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, T>) {
        let res = parser(PosStr::from(input));
        match res {
            Result::Err(_) => {},
            _ => panic!()
        }
    }

    pub fn unwrap_and_check_error<'a, T>(result: Res<'a, T>, expected: ErrorKind)
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

    pub fn check_failed<'a, T>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, T>, expected: ErrorKind)
    where T: Debug {
        let res = parser(PosStr::from(input));
        unwrap_and_check_error(res, expected);
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    // use nom::AtEof;
    #[test]
    fn parse_single_line_comment() {
        check_match_and_leftover("//foo() type if then else blaaah asdf\n aFLKdjfa ", 
        single_line_comment, PosStr::from("//foo() type if then else blaaah asdf\n"), " aFLKdjfa ");
    }
}