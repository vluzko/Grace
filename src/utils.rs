use std::str;
use std::str::from_utf8;


extern crate nom;
use self::nom::*;
use self::nom::IResult::Done as Done;
use expression::Node;

/// Map the contents of an IResult.
/// Rust functors plox
pub fn fmap_iresult<X, T, F>(res: IResult<&[u8], X>, func: F) -> IResult<&[u8], T>
    where F: Fn(X) -> T {
    return match res {
        Done(i, o) => Done(i, func(o)),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(n) => IResult::Incomplete(n)
    };
}

pub fn fmap_node<X, T, F>(res: IResult<&[u8], X>, func: F) -> IResult<&[u8], Node<T>>
    where F: Fn(X) -> T {
    return match res {
        Done(i, o) => Done(i, Node::from(func(o))),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(n) => IResult::Incomplete(n)
    };
}

pub fn fmap_convert<X>(res: IResult<&[u8], X>) -> IResult<&[u8], Node<X>> {
    return match res {
        Done(i, o) => Done(i, Node::from(o)),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(n) => IResult::Incomplete(n)
    };
}

pub fn output<T>(res: IResult<&[u8], T>) -> T {
    return match res {
        Done(_, o) => o,
        IResult::Error(e) => {
            println!("Output error: {:?}.", e);
            panic!()
        },
        IResult::Incomplete(n) => {
            println!("Incomplete: {:?}", n);
            panic!()
        }
    };
}

pub fn fmap_and_full_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    return match res {
        Done(i, o) => {
            println!("{} leftover input is {:?}. Input was: {:?}", name, from_utf8(i), from_utf8(input));
            Done(i, func(o))
        },
        IResult::Error(e) => {
            println!("{} error: {}. Input was: {:?}", name, e, from_utf8(input));
            IResult::Error(e)
        },
        IResult::Incomplete(n) => {
            println!("{} incomplete: {:?}. Input was: {:?}", name, n, from_utf8(input));
            IResult::Incomplete(n)
        }
    };
}

// TODO: Change
/// Map an IResult and log errors and incomplete values.
pub fn fmap_and_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    return match res {
        Done(i, o) => Done(i, func(o)),
        IResult::Error(e) => {
            println!("{} error: {}. Input was: {:?}", name, e, from_utf8(input));
            IResult::Error(e)
        },
        IResult::Incomplete(n) => {
            println!("{} incomplete: {:?}. Input was: {:?}", name, n, from_utf8(input));
            IResult::Incomplete(n)
        }
    };
}

pub fn full_log<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_full_log(res, |x| x, name, input);
}

pub fn log_err<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_log(res, |x| x, name, input);
}

pub fn eof_or_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return alt!(input, eof!() | tag!("\n"));
}

pub fn between_statement(input: &[u8]) -> IResult<&[u8], Vec<Vec<&[u8]>>> {
    let n = many0!(input,
        terminated!(many0!(tag!(" ")), alt!(custom_eof | tag!("\n")))
    );

    return n;
}

pub fn custom_eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return eof!(input, );
}

named!(pub inline_whitespace_char<&[u8], &[u8]>,
    tag!(" ")
);

named!(pub inline_whitespace<&[u8], Vec<&[u8]>>,
    many0!(tag!(" "))
);

/// A macro for wrapping a parser in inline whitespace.
/// Similar to ws!, but doesn't allow for \n, \r, or \t.
macro_rules! inline_wrapped (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            match tuple!($i, inline_whitespace, $submac!($($args)*), inline_whitespace) {
                IResult::Error(a)      => IResult::Error(a),
                IResult::Incomplete(i) => IResult::Incomplete(i),
                IResult::Done(remaining, (_,o, _))    => {
                    IResult::Done(remaining, o)
                }
            }
        }
    );

    ($i:expr, $f:expr) => (
        inline_wrapped!($i, call!($f));
    );
);

macro_rules! w_followed (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            match tuple!($i, $submac!($($args)*), inline_whitespace) {
                IResult::Error(a)      => IResult::Error(a),
                IResult::Incomplete(i) => IResult::Incomplete(i),
                IResult::Done(remaining, (o, _))    => {
                    IResult::Done(remaining, o)
                }
            }
        }
    );

    ($i:expr, $f:expr) => (
        w_followed!($i, call!($f));
    );
);

named!(pub valid_identifier_char<&[u8], &[u8]>,
    alt!(alpha | tag!("_") | digit)
);

/// Matches a keyword within a line.
/// Used for "and", "or", "xor", "in", etc.
macro_rules! inline_keyword (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      delimited!($i,
        inline_whitespace,
        $submac!($($args)*),
        preceded!(not!(valid_identifier_char), alt!(recognize!(many1!(inline_whitespace_char)) | peek!(tag!("(")))))
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

named!(pub ending_colon <&[u8], &[u8]>,
    terminated!(
        inline_wrapped!(tag!(":")),
        newline
    )
);


named!(pub num_follow<&[u8], &[u8]> ,
    peek!(alt!(custom_eof | tag!(" ") | tag!("(") | tag!(")") | tag!(":") | tag!("\n") | tag!(",")))
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


macro_rules! separated_at_least_m {
    ($i:expr, $m: expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => ({
        match separated_list_complete!($i, complete!($sep!($($args)*)), complete!($submac!($($args2)*))) {
            IResult::Done(i, o) => {
                if o.len() < $m {
                    IResult::Error(ErrorKind::ManyMN)
                } else {
                    IResult::Done(i, o)
                }
            },
            IResult::Error(e) => IResult::Error(e),
            IResult::Incomplete(n) => IResult::Incomplete(n)
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

/// Alias for opt!(complete!())
macro_rules! optc (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    opt!($i, complete!($submac!($($args)*)))
  );

  ($i:expr, $f:expr, $ind: expr) => (
    optc!($i, call!($f));
  );
);
