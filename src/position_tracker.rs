// use std::iter::Enumerate;
// use std::slice::Iter;
// use std::ops::{
//     Range,
//     RangeFrom,
//     RangeFull,
//     RangeTo
// };

// use bytecount;
// use memchr;
// extern crate nom;
// use self::nom::*;
// use self::nom::{
//     Compare,
//     CompareResult,
//     FindSubstring,
//     InputIter,
//     InputLength,
//     Offset,
//     Slice
// };



// pub type InputElement = u8;
// pub type Input<'a> = &'a [InputElement];
// /// Helper to declare a token.
// ///
// /// ### Examples
// ///
// /// The following example declares the `FOO_BAR` token:
// ///
// /// ```
// /// token!(FOO_BAR: b"foobar"; "The `FOO_BAR` token, mostly used in example.");
// /// ```

// /// A span is a set of meta information about a token.
// ///
// /// The `Span` structure can be used as an input of the nom parsers.
// #[derive(Debug, PartialEq, Copy, Clone)]
// pub struct Span<'a> {
//     /// The offset represents the position of the slice relatively to
//     /// the input of the parser. It starts at offset 0.
//     pub offset: usize,

//     /// The line number of the slice relatively to the input of the
//     /// parser. It starts at line 1.
//     pub line: u32,

//     /// The column number of the slice relatively to the input of the
//     /// parser. It starts at column 1.
//     pub column: u32,

//     /// The slice that is spanned.
//     slice: Input<'a>
// }

// impl<'a> Span<'a> {
//     /// Create a span for a particular input with default `offset`,
//     /// `line`, and `column` values.
//     ///
//     /// `offset` starts at 0, `line` starts at 1, and `column` starts at 1.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// use tagua_parser::tokens::Span;
//     ///
//     /// # fn main() {
//     /// let span = Span::new(b"foobar");
//     ///
//     /// assert_eq!(span.offset,     0);
//     /// assert_eq!(span.line,       1);
//     /// assert_eq!(span.column,     1);
//     /// assert_eq!(span.as_slice(), &b"foobar"[..]);
//     /// # }
//     /// ```
//     pub fn new(input: Input<'a>) -> Self {
//         Span {
//             offset: 0,
//             line  : 0,
//             column: 0,
//             slice : input
//         }
//     }

//     /// Create a span for a particular input at a particular offset, line, and column.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// use tagua_parser::tokens::Span;
//     ///
//     /// # fn main() {
//     /// let span = Span::new_at(b"foobar", 1, 2, 3);
//     ///
//     /// assert_eq!(span.offset,     1);
//     /// assert_eq!(span.line,       2);
//     /// assert_eq!(span.column,     3);
//     /// assert_eq!(span.as_slice(), &b"foobar"[..]);
//     /// # }
//     /// ```
//     pub fn new_at(input: Input<'a>, offset: usize, line: u32, column: u32) -> Self {
//         Span {
//             offset: offset,
//             line  : line,
//             column: column,
//             slice : input
//         }
//     }

//     /// Create a blank span.
//     /// This is strictly equivalent to `Span::new(b"")`.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// use tagua_parser::tokens::Span;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::empty(), Span::new(b""));
//     /// # }
//     /// ```
//     pub fn empty() -> Self {
//         Self::new(b"")
//     }

//     /// Extract the entire slice of the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// use tagua_parser::tokens::Span;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").as_slice(), &b"foobar"[..]);
//     /// # }
//     /// ```
//     pub fn as_slice(&self) -> Input<'a> {
//         self.slice
//     }
// }

// /// Implement `InputLength` from nom to be able to use the `Span`
// /// structure as an input of the parsers.
// ///
// /// This trait aims at computing the length of the input.
// impl<'a> InputLength for Span<'a> {
//     /// Compute the length of the slice in the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::InputLength;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").input_len(), 6);
//     /// # }
//     /// ```
//     fn input_len(&self) -> usize {
//         self.slice.len()
//     }
// }

// /// Implement `InputIter` from nom to be able to use the `Span`
// /// structure as an input of the parsers.
// ///
// /// This trait aims at iterating over the input.
// impl<'a> InputIter for Span<'a> {
//     /// Type of an element of the span' slice.
//     type Item     = &'a InputElement;

//     /// Type of a raw element of the span' slice.
//     type RawItem  = InputElement;

//     /// Type of the enumerator iterator.
//     type Iter     = Enumerate<Iter<'a, RawItem>>;

//     /// Type of the iterator.
//     type IterElem = Iter<'a, RawItem>;

//     /// Return an iterator that enumerates the byte offset and the
//     /// element of the slice in the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::InputIter;
//     ///
//     /// # fn main() {
//     /// let span   = Span::new(b"foobar");
//     /// let expect = vec![
//     ///     (0, b'f'),
//     ///     (1, b'o'),
//     ///     (2, b'o'),
//     ///     (3, b'b'),
//     ///     (4, b'a'),
//     ///     (5, b'r')
//     /// ];
//     ///
//     /// let mut accumulator = Vec::new();
//     ///
//     /// for (index, item) in span.iter_indices() {
//     ///     accumulator.push((index, *item));
//     /// }
//     ///
//     /// assert_eq!(accumulator, expect);
//     /// # }
//     /// ```
//     fn iter_indices(&self) -> Self::Iter {
//         self.slice.iter().enumerate()
//     }

//     /// Return an iterator over the elements of the slice in the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::InputIter;
//     ///
//     /// # fn main() {
//     /// let span   = Span::new(b"foobar");
//     /// let expect = vec![b'f', b'o', b'o', b'b', b'a', b'r'];
//     ///
//     /// let mut accumulator = Vec::new();
//     ///
//     /// for item in span.iter_elements() {
//     ///     accumulator.push(*item);
//     /// }
//     ///
//     /// assert_eq!(accumulator, expect);
//     /// # }
//     /// ```
//     fn iter_elements(&self) -> Self::IterElem {
//         self.slice.iter()
//     }

//     /// Find the byte position of an element in the slice of the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::InputIter;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").position(|x| x == b'a'), Some(4));
//     /// # }
//     /// ```
//     fn position<P>(&self, predicate: P) -> Option<usize>
//         where P: Fn(RawItem) -> bool {
//         self.slice.iter().position(|x| predicate(*x))
//     }

//     /// Get the byte offset from the element's position in the slice
//     /// of the span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::InputIter;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").slice_index(3), Some(3));
//     /// # }
//     /// ```
//     fn slice_index(&self, count: usize) -> Option<usize> {
//         if self.slice.len() >= count {
//             Some(count)
//         } else {
//             None
//         }
//     }
// }

// /// Implement `FindSubstring` from nom to be able to use the `Span`
// /// structure as an input of the parsers.
// ///
// /// This traits aims at finding a substring in an input.
// impl<'a, 'b> FindSubstring<Input<'b>> for Span<'a> {
//     /// Find the position of a substring in the current span.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::FindSubstring;
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").find_substring(b"b"), Some(3));
//     /// # }
//     /// ```
//     fn find_substring(&self, substring: Input<'b>) -> Option<usize> {
//         let substring_length = substring.len();

//         if substring_length == 0 {
//             None
//         } else if substring_length == 1 {
//             memchr::memchr(substring[0], self.slice)
//         } else {
//             let max          = self.slice.len() - substring_length;
//             let mut offset   = 0;
//             let mut haystack = self.slice;

//             while let Some(position) = memchr::memchr(substring[0], haystack) {
//                 offset += position;

//                 if offset > max {
//                     return None
//                 }

//                 if &haystack[position..position + substring_length] == substring {
//                     return Some(offset);
//                 }

//                 haystack  = &haystack[position + 1..];
//                 offset   += 1;
//             }

//             None
//         }
//     }
// }

// /// Implement `Compare` from nom to be able to use the `Span`
// /// structure as an input of the parsers.
// ///
// /// This trait aims at comparing inputs.
// impl<'a, 'b> Compare<Input<'b>> for Span<'a> {
//     /// Compare self to another input for equality.
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # extern crate tagua_parser;
//     /// # extern crate nom;
//     /// use tagua_parser::tokens::Span;
//     /// use nom::{Compare, CompareResult};
//     ///
//     /// # fn main() {
//     /// assert_eq!(Span::new(b"foobar").compare(b"foobar"), CompareResult::Ok);
//     /// # }
//     /// ```
//     fn compare(&self, element: Input<'b>) -> CompareResult {
//         self.slice.compare(element)
//     }

//     /// Compare self to another input for equality independently of the case.
//     fn compare_no_case(&self, element: Input<'b>) -> CompareResult {
//         self.slice.compare_no_case(element)
//     }
// }

// macro_rules! impl_slice_for_range {
//     ($range:ty) => (
//         /// Implement a range from nom to be able to use the `Span`
//         /// structure as an input of the parsers.
//         ///
//         /// This trait aims at slicing inputs based of a range.
//         impl<'a> Slice<$range> for Span<'a> {
//             /// Slice the span' slice based on a particular range.
//             ///
//             /// This is where new spans are computed.
//             ///
//             /// # Examples
//             ///
//             /// ```
//             /// # extern crate tagua_parser;
//             /// # extern crate nom;
//             /// use tagua_parser::tokens::Span;
//             /// use nom::Slice;
//             ///
//             /// # fn main() {
//             /// assert_eq!(
//             ///     Span::new(b"foobar").slice(2..5),
//             ///     Span::new_at(b"oba", 2, 1, 3)
//             /// );
//             /// # }
//             /// ```
//             fn slice(&self, range: $range) -> Self {
//                 let next_slice = &self.slice[range];

//                 if next_slice == self.slice {
//                     return *self;
//                 }

//                 let next_offset = self.slice.offset(next_slice);

//                 if next_offset == 0 {
//                     return Span {
//                         offset: self.offset,
//                         line  : self.line,
//                         column: self.column,
//                         slice : next_slice
//                     };
//                 }

//                 let consumed           = &self.slice[..next_offset];
//                 let number_of_newlines = bytecount::count(consumed, b'\n') as u32;

//                 let next_column =
//                     if number_of_newlines == 0 {
//                         self.column + next_offset as u32
//                     } else {
//                         match memchr::memrchr(b'\n', consumed) {
//                             Some(last_newline_position) => {
//                                 (next_offset - last_newline_position) as u32
//                             },

//                             None => 0 // unreachable
//                         }
//                     };

//                 Span {
//                     offset: self.offset + next_offset,
//                     line  : self.line + number_of_newlines,
//                     column: next_column,
//                     slice : next_slice
//                 }
//             }
//         }
//     )
// }

// impl_slice_for_range!(Range<usize>);
// impl_slice_for_range!(RangeTo<usize>);
// impl_slice_for_range!(RangeFrom<usize>);
// impl_slice_for_range!(RangeFull);

// pub fn dec_digit<'a>(input: &Span<'a>) -> IResult<Span<'a>, Span<'a>> {
//     return tag!(input, b"0");
// }

// #[cfg(test)]
// mod test {
//     use super::*;
//     #[test]
//     fn test_span() {
//         let input = Span::new(b"0");
//         let result = dec_digit(&input);
//         println!("{:?}", result);
//     }
// }