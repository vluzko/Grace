#![allow(non_snake_case)]

use std::clone::Clone;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::str;

extern crate nom;
// use self::nom;
use self::nom::*;
use super::position_tracker::PosStr;
use expression::Node;

use self::iresult_helpers::*;
use self::tokens::*;

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

/// Alias for opt!(complete!())
macro_rules! optc (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    opt!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    optc!($i, call!($f))
  );
);

/// Alias for many0!(complete!())
macro_rules! many0c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many0!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many0c!($i, call!($f))
  );
);

/// Alias for many1!(complete!())
macro_rules! many1c (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    many1!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr) => (
    many1c!($i, call!($f))
  );
);

/// Check that a macro is indented correctly.
macro_rules! indented (
  ($i:expr, $submac:ident!( $($args:tt)* ), $ind:expr) => (
    preceded!($i, complete!(many_m_n!($ind, $ind, tag!(" "))), $submac!($($args)*))
  );

  ($i:expr, $f:expr, $ind: expr) => (
    indented!($i, call!($f), $ind)
  );
);

/// A separated list with at least m elements.
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
        separated_at_least_m!($i, $m, $submac!($($args)*), call!($g))
    );
    ($i:expr, $m: expr, $f:expr, $submac:ident!( $($args:tt)* )) => (
        separated_at_least_m!($i, $m, call!($f), $submac!($($args)*))
    );
    ($i:expr, $m: expr, $f:expr, $g:expr) => (
        separated_at_least_m!($i, $m, call!($f), call!($g));
    );
}

/// A line that matches the given macro, followed by a block.
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

/// A keyword that matches the given macro, followed by a block.
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
pub fn inline_whitespace_char(input: PosStr) -> IO {
    tag!(input, " ")
}

pub fn eof_or_line(input: PosStr) -> IO {
    let val = alt!(input, eof!() | NEWLINE | EMPTY);
    val
}

/// Recognize a single line comment.
/// Single line comments can be placed anywhere a new line can be placed.
pub fn single_line_comment(input: PosStr) -> IO {
    let f = |x: u8| x == b'\n';
    recognize!(
        input,
        delimited!(tag!("//"), take_till!(f), alt!(tag!("\n") | END_OF_INPUT))
    )
}

/// Recognize any sequence of whitespace, newlines, and comments, possibly ending with end of input.
pub fn between_statement(input: PosStr) -> IResult<PosStr, PosStr> {
    let n = recognize!(
        input,
        terminated!(
            many0c!(recognize!(terminated!(
                many0c!(inline_whitespace_char),
                NEWLINE
            ))),
            optc!(END_OF_INPUT)
        )
    );

    n
}

pub mod tokens {
    use super::*;
    use expression::Identifier;

    static RESERVED_WORDS: &[&[u8]] = &[
        b"if",
        b"else",
        b"elif",
        b"for",
        b"while",
        b"and",
        b"or",
        b"not",
        b"xor",
        b"fn",
        b"import",
        b"true",
        b"false",
        b"in",
        b"match",
        b"pass",
        b"continue",
        b"break",
        b"yield",
        b"let",
        b"trait",
        b"impl",
        b"self",
    ];

    macro_rules! token {
        ($name:ident, $i: expr) => {
            pub fn $name<'a>(input: PosStr<'a>) -> IO<'a> {
                w_followed!(input, tag!($i))
            }
        };
    }

    macro_rules! keyword {
        ($name:ident, $i: expr) => {
            pub fn $name<'a>(input: PosStr<'a>) -> IO<'a> {
                w_followed!(input, terminated!(tag!($i), peek!(not!(IDENT_CHAR))))
            }
        };
    }

    /// Recognize an empty input.
    pub fn EMPTY(input: PosStr) -> IO {
        if input.input_len() == 0 {
            Ok((input, PosStr::empty()))
        } else {
            wrap_err(input, ErrorKind::NonEmpty)
        }
    }

    /// Recognize an empty input or an end of file
    pub fn END_OF_INPUT(input: PosStr) -> IO {
        if input.input_len() == 0 || input.at_eof() {
            Ok((input, PosStr::empty()))
        } else {
            wrap_err(input, ErrorKind::NonEmpty)
        }
    }

    pub fn NUM_START(input: PosStr) -> IO {
        match input.slice.len() {
            0 => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
            _ => {
                let x = input.slice[0];
                match (0x30..=0x39).contains(&x) || x == 0x2e {
                    true => Ok(input.take_split(1)),
                    false => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
                }
            }
        }
    }

    /// Recognize a non-empty sequence of decimal digits.
    pub fn DIGIT(input: PosStr) -> IO {
        match input.position(|x| !(0x30..=0x39).contains(&x)) {
            Some(0) => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input, ErrorKind::Digit),
                n => Ok(input.take_split(n)),
            },
        }
    }

    /// Recognize a sequence of decimal digits.
    pub fn DIGIT0(input: PosStr) -> IO {
        match input.position(|x| !(0x30..=0x39).contains(&x)) {
            Some(n) => Ok(input.take_split(n)),
            None => Ok(input.take_split(input.input_len())),
        }
    }

    /// Recognize a sequence of (ASCII) alphabetic characters.
    pub fn ALPHA(input: PosStr) -> IO {
        match input.position(|x| !((65..=90).contains(&x) || (97..=122).contains(&x))) {
            Some(0) => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input, ErrorKind::Digit),
                n => Ok(input.take_split(n)),
            },
        }
    }

    /// Recognize a sequence of alphanumeric characters.
    pub fn ALPHANUM(input: PosStr) -> IO {
        match input.position(|x: u8| !x.is_alpha() && !x.is_dec_digit()) {
            Some(0) => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input, ErrorKind::Digit),
                n => Ok(input.take_split(n)),
            },
        }
    }

    /// Recognize a sequence of valid internal identifier characters.
    pub fn IDENT_CHAR(input: PosStr) -> IO {
        let identifier_segment = input.position(|x| {
            let y = x.as_char();
            !(y.is_alpha() || y.is_dec_digit() || y == '_')
        });
        match identifier_segment {
            Some(0) => Err(Err::Error(Context::Code(input, ErrorKind::Digit))),
            Some(n) => Ok(input.take_split(n)),
            None => match input.input_len() {
                0 => wrap_err(input, ErrorKind::Digit),
                n => Ok(input.take_split(n)),
            },
        }
    }

    /// Recognize a character in a string, or an escape sequence.
    pub fn STRING_CHAR(input: PosStr) -> IO {
        alt!(
            input,
            tag!("\\\"")
                | tag!("\\\'")
                | tag!("\\\\")
                | tag!("\\n")
                | tag!("\\\r")
                | recognize!(none_of!("\r\n\"\\"))
        )
    }

    /// true if a key
    pub fn RESERVED(input: PosStr) -> IO {
        let tag_lam = |x| recognize!(input, complete!(tag!(PosStr::new(x))));
        let tag_iter = RESERVED_WORDS.iter().map(|x| x.as_bytes()).map(tag_lam);
        let mut final_result: IO = wrap_err(input, ErrorKind::Tag);
        for res in tag_iter {
            match res {
                Ok((i, o)) => {
                    final_result = Ok((i, o));
                    break;
                }
                _ => continue,
            };
        }
        final_result
    }

    /// Parser to an Identifier AST.
    pub fn IDENTIFIER(input: PosStr) -> IResult<PosStr, Identifier> {
        let parse_result = w_followed!(
            input,
            recognize!(pair!(alt!(ALPHA | tag!("_")), optc!(IDENT_CHAR)))
        );
        let intermediate = match parse_result {
            Ok((i, o)) => match RESERVED_WORDS.iter().find(|x| *x == &(o.slice)) {
                Some(_) => wrap_err(i, ErrorKind::Alt),
                None => Ok((i, o)),
            },
            Err(x) => Err(x),
        };
        fmap_iresult(intermediate, Identifier::from)
    }

    pub fn VALID_NUM_FOLLOW(input: PosStr) -> IO {
        if input.input_len() == 0 {
            Ok((input, input))
        } else {
            peek!(
                input,
                alt!(
                    eof!()
                        | tag!(" ")
                        | tag!("(")
                        | tag!(")")
                        | tag!(":")
                        | tag!("\n")
                        | tag!(",")
                        | tag!("]")
                        | tag!("}")
                        | tag!("=>")
                        | tag!("+")
                        | tag!("-")
                        | tag!("*")
                        | tag!("/")
                        | tag!("%")
                        | tag!("&")
                        | tag!("|")
                        | tag!("^")
                        | tag!("!")
                        | tag!("=")
                        | tag!("<")
                        | tag!(">")
                )
            )
        }
    }

    pub fn NEWLINE(input: PosStr) -> IO {
        alt_complete!(input, tag!("\n") | single_line_comment)
    }

    pub fn SIGN(input: PosStr) -> IO {
        recognize!(input, alt!(tag!("+") | tag!("-")))
    }

    /// Parser for the negative unary operator. Checks that it's not immediately followed by a digit.
    pub fn NEG(input: PosStr) -> IO {
        w_followed!(input, terminated!(tag!("-"), peek!(not!(NUM_START))))
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
    where
        F: Fn(PosStr<'a>) -> Res<'a, T>,
    {
        match res {
            Ok((i, o)) => match parser(i) {
                Ok((i_final, parser_o)) => Ok((i_final, (o, parser_o))),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }

    /// Map the contents of an IResult.
    /// Rust functors plox
    pub fn fmap_iresult<X, T, F>(res: Res<X>, func: F) -> Res<T>
    where
        F: Fn(X) -> T,
    {
        match res {
            Ok((i, o)) => Ok((i, func(o))),
            Err(e) => Err(e),
        }
    }

    pub fn fmap_update<X, U, T, F>(res: Res<(X, U)>, func: F) -> Res<(T, U)>
    where
        F: Fn(X) -> T,
    {
        match res {
            Ok((i, (o, u))) => Ok((i, (func(o), u))),
            Err(e) => Err(e),
        }
    }

    /// Map data and wrap the result in a Node.
    /// * `res`     - The parser result to map.
    /// * `func`    - The mapping function.
    /// * `start`   - The line and column of the start of the match that produced the data.
    pub fn fmap_node<'a, X, T, F>(res: Res<'a, X>, func: F, start: &(u32, u32)) -> Res<'a, Node<T>>
    where
        F: Fn(X) -> T,
    {
        match res {
            Ok((i, o)) => Ok((i, Node::from((func(o), start.0, start.1, i.line, i.column)))),
            Err(e) => Err(e),
        }
    }

    /// Map data and update with a function that does not produce an additional update, and wrap the result in a Node.
    /// * `res`     - The parser result to map.
    /// * `func`    - The mapping function.
    /// * `start`   - The line and column of the start of the match that produced the data.
    pub fn fmap_pass<'a, X, U, T, F>(
        res: Res<'a, (X, U)>,
        func: F,
        start: &(u32, u32),
    ) -> Res<'a, (Node<T>, U)>
    where
        F: Fn(X) -> T,
    {
        match res {
            Ok((i, o)) => Ok((
                i,
                (
                    Node::from((func(o.0), start.0, start.1, i.line, i.column)),
                    o.1,
                ),
            )),
            Err(e) => Err(e),
        }
    }

    /// Map data and update with a function that may produce an additional update, and wrap the result in a Node.
    /// * `res`     - The parser result to map.
    /// * `func`    - The mapping function.
    /// * `start`   - The line and column of the start of the match that produced the data.
    pub fn fmap_nodeu<'a, X, U, T, F>(
        res: Res<'a, X>,
        func: F,
        start: &(u32, u32),
    ) -> Res<'a, (Node<T>, U)>
    where
        F: Fn(X) -> (T, U),
    {
        match res {
            Ok((i, o)) => {
                let (v, u) = func(o);
                Ok((i, (Node::from((v, start.0, start.1, i.line, i.column)), u)))
            }
            Err(e) => Err(e),
        }
    }

    pub fn wrap_err<T>(input: PosStr, error: ErrorKind) -> Res<T> {
        Err(Err::Error(Context::Code(input, error)))
    }

    pub fn output<T>(res: Res<T>) -> T {
        match res {
            Ok((_, o)) => o,
            Err(e) => panic!("Output error: {:?}.", e),
        }
    }

    pub fn check_match_and_leftover<'a, T>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, T>,
        expected: T,
        expected_leftover: &'a str,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                assert_eq!(i.slice, expected_leftover.as_bytes());
                assert_eq!(o, expected);
            }
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    pub fn check_data_and_leftover<'a, T, U>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, (Node<T>, U)>,
        expected: T,
        expected_leftover: &str,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                assert_eq!(i.slice, expected_leftover.as_bytes());
                assert_eq!(o.0.data, expected);
            }
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    /// Check that the parser applied to the input returns the expect result.
    pub fn check_match<'a, T>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, T>,
        expected: T,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(
                    i.slice, b"",
                    "Leftover input should have been empty, was: {:?}\nResults were: {}",
                    i, l_r
                );
                assert_eq!(o, expected);
            }
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    pub fn check_match_no_update<'a, T, U>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, (T, U)>,
        expected: T,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.0);
                assert_eq!(
                    i.slice, b"",
                    "Leftover input should have been empty, was: {:?}\nResults were: {}",
                    i, l_r
                );
                assert_eq!(o.0, expected);
            }
            Result::Err(e) => {
                panic!("Error: {:?}. Input was: {:?}", e, input)
            }
        }
    }

    /// Check just the data of the result of a parser that includes an update. Skips the containing node and the update.
    pub fn check_data<'a, T, U>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, (Node<T>, U)>,
        expected: T,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.0.data);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}\nInput was: {}", i, l_r, input);
                assert_eq!(
                    o.0.data, expected,
                    "Results were: {}\nInput was: {}",
                    l_r, input
                );
            }
            Result::Err(e) => {
                panic!("Error: {:?}.\nInput was: {}", e, input)
            }
        };
    }

    /// Check just the data of the result of a parser. Skips the containing node.
    pub fn check_data_no_update<'a, T>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, Node<T>>,
        expected: T,
    ) where
        T: Debug + PartialEq + Eq,
    {
        let res = parser(PosStr::from(input));
        match res {
            Ok((i, o)) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o.data);
                assert_eq!(i.slice, b"", "Leftover input should have been empty, was: {:?}\nResults were: {}\nInput was: {}", i, l_r, input);
                assert_eq!(
                    o.data, expected,
                    "Results were: {}\nInput was: {}",
                    l_r, input
                );
            }
            Result::Err(e) => {
                panic!("Error: {:?}.\nInput was: {}", e, input)
            }
        };
    }

    pub fn simple_check_failed<'a, T>(input: &'a str, parser: impl Fn(PosStr<'a>) -> Res<'a, T>) {
        let res = parser(PosStr::from(input));
        match res {
            Result::Err(_) => {}
            _ => panic!(),
        }
    }

    pub fn unwrap_and_check_error<T>(result: Res<T>, expected: ErrorKind)
    where
        T: Debug,
    {
        match result {
            Result::Err(e) => match e {
                Err::Error(actual_err) => match actual_err {
                    Context::Code(_, actual_err) => assert_eq!(actual_err, expected),
                },
                _ => panic!(),
            },
            Ok(x) => {
                panic!("Should have failed, got: {:?}", x)
            }
        }
    }

    pub fn check_failed<'a, T>(
        input: &'a str,
        parser: impl Fn(PosStr<'a>) -> Res<'a, T>,
        expected: ErrorKind,
    ) where
        T: Debug,
    {
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
        check_match_and_leftover(
            "//foo() type if then else blaaah asdf\n aFLKdjfa ",
            single_line_comment,
            PosStr::from("//foo() type if then else blaaah asdf\n"),
            " aFLKdjfa ",
        );
    }

    #[test]
    fn parse_new_line() {
        check_match("\n", NEWLINE, PosStr::from("\n"));
        check_match_and_leftover("\n ", NEWLINE, PosStr::from("\n"), " ");
    }

    #[test]
    fn parse_eof_or_line() {
        check_match("\n", eof_or_line, PosStr::from("\n"));
        check_match("", eof_or_line, PosStr::from(""));
    }

    #[test]
    fn parse_between_stmt() {
        check_match("\n", between_statement, PosStr::from("\n"));
        check_match("    \n", between_statement, PosStr::from("    \n"));
        check_match(
            "   \n  \n  \n",
            between_statement,
            PosStr::from("   \n  \n  \n"),
        );
        check_match_and_leftover(
            "   \n  \n  \n   ",
            between_statement,
            PosStr::from("   \n  \n  \n"),
            "   ",
        );
    }
}
