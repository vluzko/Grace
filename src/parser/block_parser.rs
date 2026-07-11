extern crate nom;
use self::nom::*;
use crate::expression::*;
use crate::parser::base::ParserContext;
use crate::parser::parser_utils::iresult_helpers::*;
use crate::parser::parser_utils::*;
use crate::parser::position_tracker::PosStr;
use nom::Parser;
use nom::bytes::complete::tag;
use nom::combinator::opt;
use nom::error::ErrorKind;
use nom::multi::{many0, many1};
use nom::sequence::{preceded, terminated};

/// Parser for blocks
impl ParserContext {
    pub fn block<'a>(&self, input: PosStr<'a>, indent: usize) -> IResult<PosStr<'a>, Node<Block>> {
        let first_indent_parse = preceded(opt(between_statement), many0(tag(" "))).parse(input);
        let full_indent = match first_indent_parse {
            Ok((i, o)) => (i, o),
            _ => panic!(),
        };

        // Break if the block is not indented enough.
        let parse_result = if full_indent.1.len() < indent {
            // TODO: This will happen if the indentation level is too low. Throw a proper error.
            Result::Err(Err::Error(nom::error::Error::new(input, ErrorKind::Count)))
        } else {
            let expected_indent = full_indent.1.len();
            // We end up reparsing the initial indent, but that's okay. The alternative is joining two
            // vectors, which is much slower.
            // TODO: See if we can clean this up with a separated_list_complete.

            preceded(
                opt(between_statement),
                many1(terminated(
                    |i| indented!(i, |j| self.statement(j, expected_indent), expected_indent),
                    between_statement,
                )),
            )
            .parse(input)
        };

        fmap_node(
            parse_result,
            |x| {
                let mut statements = vec![];
                // Add all the updates to the block. Updates always go before the statement that created them.
                for (stmt, mut update) in x {
                    statements.append(&mut update);
                    statements.push(Box::new(stmt));
                }
                Block { statements }
            },
            &(input.line, input.column),
        )
    }
}

#[cfg(test)]
mod property_based_tests {
    use super::*;
    use crate::testing::proptest_utils::strategies;
    use proptest::prelude::*;

    // Check that blocks of simple statements can parse at all.
    proptest! {
        #[test]
        fn prop_simple_block_parses(v in strategies::simple_block_strategy()) {
            let block_string: String = v
                .statements
                .iter()
                .map(|s| format!(" {}\n", s.data.inverse_parse()))
                .collect();
            let e = ParserContext::empty();
            let result = e.block(PosStr::from(block_string.as_bytes()), 1);
            result.unwrap();
        }
    }
}
