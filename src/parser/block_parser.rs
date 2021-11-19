extern crate nom;
use self::nom::*;
use expression::*;
use parser::base::ParserContext;
use parser::parser_utils::iresult_helpers::*;
use parser::parser_utils::*;
use parser::position_tracker::PosStr;

/// Parser for blocks
impl ParserContext {
    pub fn block<'a>(&self, input: PosStr<'a>, indent: usize) -> IResult<PosStr<'a>, Node<Block>> {
        let first_indent_parse = preceded!(input, opt!(between_statement), many0c!(tag!(" ")));
        let full_indent = match first_indent_parse {
            Ok((i, o)) => (i, o),
            _ => panic!(),
        };

        // Break if the block is not indented enough.
        let parse_result = if full_indent.1.len() < indent {
            // TODO: This will happen if the indentation level is too low. Throw a proper error.
            Result::Err(Err::Error(Context::Code(input, ErrorKind::Count)))
        } else {
            let expected_indent = full_indent.1.len();
            // We end up reparsing the initial indent, but that's okay. The alternative is joining two
            // vectors, which is much slower.
            // TODO: See if we can clean this up with a separated_list_complete.
            let statements = preceded!(
                input,
                opt!(between_statement),
                many1!(complete!(terminated!(
                    indented!(m!(self.statement, expected_indent), expected_indent),
                    between_statement
                )))
            );
            statements
        };

        return fmap_node(parse_result, |x| {
            let mut statements = vec![];
            // Add all the updates to the block. Updates always go before the statement that created them.
            for (stmt, mut update) in x {
                statements.append(&mut update);
                statements.push(Box::new(stmt));
            }
            Block { statements }
        });
    }
}
