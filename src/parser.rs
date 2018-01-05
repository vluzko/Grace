use error::*;
use std::str;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::env;

extern crate nom;
use self::nom::*;
//use nom::Offset;

named!(string_between_quotes, delimited!(char!('\"'), is_not!("\""), char!('\"')));
named!(get_cell, take_while!(is_not_cell_end));
named!(consume_useless_chars, take_while!(is_whitespace));

macro_rules! separated_list2 (
  ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => (
    {
      let mut res   = ::std::vec::Vec::new();
      let mut input = $i;

      // get the first element
      let first = $submac!(input, $($args2)*);

      if let nom::IResult::Done(i, o) = first {
         if i.len() == input.len() {
            //let err : nom::IResult<&[u8], Vec<Vec<String>>, CsvError> = nom::IResult::Error(Err::Position(nom::ErrorKind::SeparatedList, input)); err
            let err : nom::IResult<&[u8], Vec<Vec<String>>, CsvError> = nom::IResult::Error(nom::ErrorKind::SeparatedList); err
          } else {
            res.push(o);
            input = i;

            loop {
              // get the separator first
              if let nom::IResult::Done(i2,_) = $sep!(input, $($args)*) {
                if i2.len() == input.len() {
                  break;
                }
                input = i2;

                // get the element next
                if let nom::IResult::Done(i3,o3) = $submac!(input, $($args2)*) {
                  res.push(o3);
                  input = i3;
                  if i3.len() == input.len() {
                    break;
                  }
                } else {
                  break;
                }
              } else {
                break;
              }
            }
            nom::IResult::Done(input, res)
          }
      } else if let nom::IResult::Incomplete(i) = first {
        nom::IResult::Incomplete(i)
      } else {
        nom::IResult::Done(input, ::std::vec::Vec::new())
      }
    }
  );
  ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => (
    separated_list!($i, $submac!($($args)*), call!($g));
  );
  ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => (
    separated_list!($i, call!($f), $submac!($($args)*));
  );
  ($i:expr, $f:expr, $g:expr) => (
    separated_list!($i, call!($f), call!($g));
  );
);

fn is_whitespace(c: u8) -> bool {
    c as char == ' ' || c as char == '\t'
}

fn is_not_cell_end(c: u8) -> bool {
    c as char != ',' && c as char != '\n'
}

fn get_column_value(input: &[u8], pos: Position) -> nom::IResult<&[u8], &[u8], CsvError> {
    let (i, cell) = try_parse!(input,
        fix_error!(CsvError,
            preceded!(
            opt!(consume_useless_chars),
                alt!(
                    string_between_quotes | get_cell
                )
            )
        )
    );

    if i.len() == 0 {
        //nom::IResult::Incomplete(Needed::Unknown)
        nom::IResult::Done(i, cell)
    } else if is_not_cell_end(i[0]) {
        let p = Position { line: pos.line, column: pos.column + input.offset(i) };
        //nom::IResult::Error(Err::Code(ErrorKind::Custom(
        //    CsvInvalidCharacter(Charnew(',', i[0] as char, &p))
        //)))
        nom::IResult::Error(nom::ErrorKind::Custom(
            CsvError::InvalidCharacter(CharError::new(',', i[0] as char, &p))
        ))
    } else {
        nom::IResult::Done(i, cell)
    }
}

fn get_string_column_value(input: &[u8], pos: Position) -> nom::IResult<&[u8], String, CsvError> {
    map_res!(input,
        map_res!(
            dbg_dmp!(
                apply!(get_column_value, Position::new(pos.line, pos.column))
            ),
            from_utf8
        ),
        |d| {
            str::FromStr::from_str(d)
        }
    )
}

fn comma_then_column<'a>(input: &'a [u8], pos: &Position) -> nom::IResult<&'a [u8], String, CsvError> {
    preceded!(input,
        fix_error!(CsvError, char!(',')),
        apply!(get_string_column_value, Position::new(pos.line, pos.column))
    )
}

fn many_comma_then_column(input: &[u8], pos: Position) -> nom::IResult<&[u8], Vec<String>, CsvError> {
    many0!(
        input,
        apply!(comma_then_column, &pos)
    )
}

fn get_line_values<'a>(entry: &'a[u8], ret: &mut Vec<String>, line: usize) -> nom::IResult<&'a[u8], &'a[u8], CsvError> {
    if entry.len() == 0 {
        nom::IResult::Done(entry, entry)
    } else {
        let (i, col) = try_parse!(entry, apply!(get_string_column_value, Position::new(line, ret.len())));
        ret.push(col);

        match fix_error!(i, CsvError, separated_list2!(
            char!('\n'),
            apply!(many_comma_then_column, Position::new(line, ret.len()))
        )) {
            nom::IResult::Done(i, v)    => {
                let v : Vec<Vec<String>> = v;
                for c in v {
                    for sub_c in c {
                        ret.push(sub_c);
                    }
                }
                nom::IResult::Done(i, &entry[..entry.offset(i)])
            },
            nom::IResult::Incomplete(i) => nom::IResult::Incomplete(i),
            nom::IResult::Error(e)      => nom::IResult::Error(e)
        }
    }
}

fn get_lines_values(mut ret: Vec<Vec<String>>, entry: &[u8]) -> Result<Vec<Vec<String>>, CsvError> {
    let mut input = entry;
    let mut line  = 0;
    loop {
        let mut v: Vec<String> = Vec::new();
        match get_line_values(input, &mut v, line) {
            //nom::IResult::Error(Err::Code(ErrorKind::Custom(e))) => return Err(e),
            nom::IResult::Error(_)                               => return Err(CsvError::GenericError),
            nom::IResult::Incomplete(_)                          => {
                // did we reach the end of file?
                break
            }
            nom::IResult::Done(i,_)                              => {
                input = i;
                line += 1;
                ret.push(v);
                if input.len() == 0 {
                    break;
                }
            },
        }
    }

    Ok(ret)
}

pub fn parse_csv_from_slice(entry: &[u8]) -> Result<Vec<Vec<String>>, CsvError> {
    get_lines_values(vec!(), entry)
}

pub fn parse_csv_from_file(filename: &str) -> Result<Vec<Vec<String>>, CsvError> {
    let mut f = File::open(filename).unwrap();
    let mut buffer = vec!();

    f.read_to_end(&mut buffer).unwrap();
    parse_csv_from_slice(&buffer)
}

pub fn parse_csv(entry: &str) -> Result<Vec<Vec<String>>, CsvError> {
    parse_csv_from_slice(entry.as_bytes())
}

//#[test]
fn check_string_between_quotes() {
    let f = b"\"nom\",age\ncarles,30\nlaure,28\n";

    match string_between_quotes(f) {
        nom::IResult::Done(in_, out) => {
            assert_eq!(out, b"nom");
            assert_eq!(in_, b",age\ncarles,30\nlaure,28\n");
        },
        nom::IResult::Incomplete(x) => panic!("incomplete: {:?}", x),
        nom::IResult::Error(e) => panic!("error: {:?}", e),
    }
}

//#[test]
pub fn check_get_cell() {
    let f = b"age\ncarles,30\n";
    let g = b"age2,carles,30\n";

    match get_cell(f) {
        nom::IResult::Done(_, out) => assert_eq!(out, b"age"),
        nom::IResult::Incomplete(x) => panic!("incomplete: {:?}", x),
        nom::IResult::Error(e) => panic!("error: {:?}", e),
    }
    match get_cell(g) {
        nom::IResult::Done(_, out) => assert_eq!(out, b"age2"),
        nom::IResult::Incomplete(x) => panic!("incomplete: {:?}", x),
        nom::IResult::Error(e) => panic!("error: {:?}", e),
    }
}

//#[test]
pub fn check_get_line_values() {
    // no terminator, this is not a line
    //let mut cells = vec!();
    //get_line_values(&mut cells, b"\"nom\",,age", 0);
    //assert_eq!(cells, vec!("nom".to_owned(), "".to_owned(), "age".to_owned()));

    let mut cells = vec!();
    let res = get_line_values(b"\"nom\",,age\n", &mut cells, 0);
    println!("res: {:?}", res);
    assert_eq!(cells, vec!("nom".to_owned(), "".to_owned(), "age".to_owned()));

    let mut cells = vec!();
    get_line_values(b"\"nom\",age,\n", &mut cells, 0);
    assert_eq!(cells, vec!("nom".to_owned(), "age".to_owned(), "".to_owned()));

    let mut cells = vec!();
    get_line_values(b"\"nom\",age,,\"hoho\",,end\n", &mut cells, 0);
    assert_eq!(cells, vec!("nom".to_owned(), "age".to_owned(), "".to_owned(), "hoho".to_owned(), "".to_owned(), "end".to_owned()));

    let mut cells = vec!();
    let e = get_line_values(b"\"nom\" ,age,\"hoho\"", &mut cells, 0);
//    assert_eq!(e, nom::IResult::Error(Err::Code(ErrorKind::Custom(
//                   CsvError::InvalidCharacter(CharError::new(',', ' ', &Position::new(0, 5)))
//               )))
//    );
}

//#[test]
pub fn check_get_lines_values() {
    let f = b"\"nom\",age\ncarles,30\nlaure,28\n";

    assert_eq!(get_lines_values(vec!(), f),
               Ok(vec!(
                   vec!("nom".to_owned(), "age".to_owned()),
                   vec!("carles".to_owned(), "30".to_owned()),
                   vec!("laure".to_owned(), "28".to_owned()))));
    let f = b"\"nom\",age\ncarles,30\nlaure,28";

    assert_eq!(get_lines_values(vec!(), f),
               Ok(vec!(
                   vec!("nom".to_owned(), "age".to_owned()),
                   vec!("carles".to_owned(), "30".to_owned()),
                   vec!("laure".to_owned(), "28".to_owned()))));
}

//#[test]
pub fn check_parse_csv() {
    let f = "\"nom\",age\ncarles,30\nlaure,28\n";

    assert_eq!(parse_csv(f),
               Ok(vec!(
                   vec!("nom".to_owned(), "age".to_owned()),
                   vec!("carles".to_owned(), "30".to_owned()),
                   vec!("laure".to_owned(), "28".to_owned()))));
}

#[test]
pub fn basic_file_test() {
    // Read file
    let filename= "./test_data/simple_grace.gr";
    let mut f = File::open(filename).expect("File not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("Problem reading file.");
    println!("{}", contents);

    // "parse" file
    let results = parse_csv(&contents);
    // Print parsing result
    println!("Result: {:?}", results);
}

