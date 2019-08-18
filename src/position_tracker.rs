use std::iter::Enumerate;
use std::slice::Iter;
use std::ops::{
    Range,
    RangeFrom,
    RangeFull,
    RangeTo
};

use bytecount;
use memchr;
extern crate nom;
// use self::nom::*;
use self::nom::{
    Compare,
    CompareResult,
    FindSubstring,
    InputIter,
    InputLength,
    Offset,
    Slice,
    AtEof,
    InputTake
};

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct PosStr<'a> {
    /// The offset represents the position of the slice relatively to
    /// the input of the parser. It starts at offset 0.
    pub offset: usize,

    /// The line number of the slice relatively to the input of the
    /// parser. It starts at line 1.
    pub line: u32,

    /// The column number of the slice relatively to the input of the
    /// parser. It starts at column 1.
    pub column: u32,

    /// The slice that is spanned.
    slice: &'a [u8]
}

impl<'a> PosStr<'a> {

    pub fn new(input: &'a [u8]) -> Self {
        PosStr {
            offset: 0,
            line  : 0,
            column: 0,
            slice : input
        }
    }

    pub fn new_at(input: &'a [u8], offset: usize, line: u32, column: u32) -> Self {
        PosStr {
            offset: offset,
            line  : line,
            column: column,
            slice : input
        }
    }


    pub fn empty() -> Self {
        Self::new(b"")
    }


    pub fn as_slice(&self) -> &'a [u8] {
        self.slice
    }
}

impl <'a> From<&'a [u8]> for PosStr<'a> {
    fn from(input: &'a [u8]) -> Self {
        return PosStr {
            offset: 0,
            line  : 0,
            column: 0,
            slice : input
        }
    }
}

impl<'a> InputLength for PosStr<'a> {
    fn input_len(&self) -> usize {
        self.slice.len()
    }
}

impl<'a> InputIter for PosStr<'a> {
    /// Type of an element of the PosStr' slice.
    type Item     = &'a u8;

    /// Type of a raw element of the PosStr' slice.
    type RawItem  = u8;

    /// Type of the enumerator iterator.
    type Iter     = Enumerate<Iter<'a, Self::RawItem>>;

    /// Type of the iterator.
    type IterElem = Iter<'a, Self::RawItem>;


    fn iter_indices(&self) -> Self::Iter {
        self.slice.iter().enumerate()
    }


    fn iter_elements(&self) -> Self::IterElem {
        self.slice.iter()
    }


    fn position<P>(&self, predicate: P) -> Option<usize>
        where P: Fn(Self::RawItem) -> bool {
        self.slice.iter().position(|x| predicate(*x))
    }


    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.slice.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a, 'b> FindSubstring<&'b [u8]> for PosStr<'a> {

    fn find_substring(&self, substring: &'b [u8]) -> Option<usize> {
        let substring_length = substring.len();

        if substring_length == 0 {
            None
        } else if substring_length == 1 {
            memchr::memchr(substring[0], self.slice)
        } else {
            let max          = self.slice.len() - substring_length;
            let mut offset   = 0;
            let mut haystack = self.slice;

            while let Some(position) = memchr::memchr(substring[0], haystack) {
                offset += position;

                if offset > max {
                    return None
                }

                if &haystack[position..position + substring_length] == substring {
                    return Some(offset);
                }

                haystack  = &haystack[position + 1..];
                offset   += 1;
            }

            None
        }
    }
}

impl<'a, 'b> Compare<&'b [u8]> for PosStr<'a> {

    fn compare(&self, element: &'b [u8]) -> CompareResult {
        self.slice.compare(element)
    }

    /// Compare self to another input for equality independently of the case.
    fn compare_no_case(&self, element: &'b [u8]) -> CompareResult {
        self.slice.compare_no_case(element)
    }
}

impl<'a, 'b> Compare<&'b str> for PosStr<'a> {

    fn compare(&self, element: &'b str) -> CompareResult {
        self.slice.compare(element)
    }

    /// Compare self to another input for equality independently of the case.
    fn compare_no_case(&self, element: &'b str) -> CompareResult {
        self.slice.compare_no_case(element)
    }
}

impl<'a, 'b> Compare<PosStr<'b>> for PosStr<'a> {

    fn compare(&self, element: PosStr<'b>) -> CompareResult {
        self.slice.compare(element.slice)
    }

    /// Compare self to another input for equality independently of the case.
    fn compare_no_case(&self, element: PosStr<'b>) -> CompareResult {
        self.slice.compare_no_case(element.slice)
    }
}

impl <'a> AtEof for PosStr<'a> {
  fn at_eof(&self) -> bool {
    return self.slice.at_eof();
  }
}

impl <'a> InputTake for PosStr<'a> {
    fn take(&self, count: usize) -> Self {
        let new_slice = self.slice.take(count);
        let new_pos_str = PosStr {
            offset: self.offset,
            line: self.line, 
            column: self.column,
            slice: new_slice
          };

        return new_pos_str;
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (second_slice, first_slice) = self.slice.take_split(count);
        let first_pos_str = PosStr {
            offset: self.offset,
            line: self.line, 
            column: self.column,
            slice: first_slice
        };
        let number_of_newlines = bytecount::count(first_slice, b'\n') as u32;
        let next_offset = self.slice.offset(second_slice);
        let next_column = if number_of_newlines == 0 {
            self.column + next_offset as u32
        } else {
            match memchr::memrchr(b'\n', &first_slice) {
                Some(last_newline_position) => {
                    (next_offset - last_newline_position) as u32
                },
                None => 0 // unreachable
            }
        };

        let second_pos_str = PosStr {
            offset: next_offset,
            line: self.line + number_of_newlines,
            column: next_column,
            slice: second_slice
        };

        return (second_pos_str, first_pos_str);
    }
}

impl <'a> Offset for PosStr<'a> {
    fn offset(&self, second: &Self) -> usize {
        return self.slice.offset(second.slice);
    }
}

macro_rules! impl_slice_for_range {
    ($range:ty) => (
        impl<'a> Slice<$range> for PosStr<'a> {

            fn slice(&self, range: $range) -> Self {
                let next_slice = &self.slice[range];

                if next_slice == self.slice {
                    return *self;
                }

                let next_offset = self.slice.offset(next_slice);

                if next_offset == 0 {
                    return PosStr {
                        offset: self.offset,
                        line  : self.line,
                        column: self.column,
                        slice : next_slice
                    };
                }

                let consumed           = &self.slice[..next_offset];
                let number_of_newlines = bytecount::count(consumed, b'\n') as u32;

                let next_column = if number_of_newlines == 0 {
                  self.column + next_offset as u32
                } else {
                    match memchr::memrchr(b'\n', consumed) {
                      Some(last_newline_position) => {
                        (next_offset - last_newline_position) as u32
                      },

                      None => 0 // unreachable
                    }
                };

                PosStr {
                    offset: self.offset + next_offset,
                    line  : self.line + number_of_newlines,
                    column: next_column,
                    slice : next_slice
                }
            }
        }
    )
}

impl_slice_for_range!(Range<usize>);
impl_slice_for_range!(RangeTo<usize>);
impl_slice_for_range!(RangeFrom<usize>);
impl_slice_for_range!(RangeFull);

#[cfg(test)]
mod test {
}