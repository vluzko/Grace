//! Module level parser.
//! Parses imports and function declarations.
extern crate nom;
use self::nom::*;
use crate::expression::*;
use crate::parser::base::{ParserContext, Res};
use crate::parser::parser_utils::iresult_helpers::*;
use crate::parser::parser_utils::tokens::*;
use crate::parser::parser_utils::*;
use crate::parser::position_tracker::PosStr;
use nom::Parser;
use nom::branch::alt;
use nom::combinator::{eof, map, opt};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{preceded, terminated};
use std::collections::HashMap;

use crate::general_utils::get_next_id;
use crate::type_checking::types::Trait;

/// Parse a module.
pub fn module(input: PosStr) -> IResult<PosStr, Node<Module>> {
    let mut context = ParserContext::empty();

    enum ModuleDec {
        Func(Node<Stmt>),
        Struct(Node<Stmt>),
        TraitDec(Trait),
        TraitImpl((Identifier, Identifier, Vec<Node<Stmt>>)),
    }

    let just_imports = preceded(
        opt(between_statement),
        many0(terminated(import, between_statement)),
    )
    .parse(input);

    let (remaining, imports) = match &just_imports {
        Ok((i, parsed_imports)) => {
            for import in parsed_imports {
                match &import.alias {
                    Some(alias) => context.imported.insert(alias.clone(), import.clone()),
                    None => context
                        .imported
                        .insert(import.path.first().unwrap().clone(), import.clone()),
                };
            }
            (i, parsed_imports)
        }
        _ => panic!(),
    };

    let w_self_t = ParserContext {
        imported: context.imported.clone(),
        can_use_self: true,
    };
    let declarations = terminated(
        many1(terminated(
            alt((
                map(|i| context.function_declaration_stmt(i, 0), ModuleDec::Func),
                map(|i| context.struct_declaration_stmt(i), ModuleDec::Struct),
                map(|i| w_self_t.trait_parser(i), ModuleDec::TraitDec),
                map(|i| w_self_t.trait_impl(i), ModuleDec::TraitImpl),
            )),
            between_statement,
        )),
        alt((eof, EMPTY)),
    )
    .parse(*remaining);

    fmap_node(
        declarations,
        |just_decs| {
            let mut traits = HashMap::new();
            let mut funcs = vec![];
            let mut structs = vec![];
            let mut trait_impls = vec![];

            for d in just_decs {
                match d {
                    ModuleDec::Func(x) => {
                        funcs.push(Box::new(x));
                    }
                    ModuleDec::Struct(x) => {
                        structs.push(Box::new(x));
                    }
                    ModuleDec::TraitDec(x) => {
                        traits.insert(x.name.clone(), x);
                    }
                    ModuleDec::TraitImpl(x) => {
                        trait_impls.push(x);
                    }
                };
            }

            Module {
                imports: imports.iter().map(|z| Box::new(z.clone())).collect(),
                functions: funcs,
                structs,
                traits,
                trait_implementations: trait_impls,
            }
        },
        &(input.line, input.column),
    )
}

/// Parse an import statement.
pub(in crate::parser) fn import(input: PosStr) -> Res<Import> {
    let parse_result = preceded(
        IMPORT,
        (
            separated_list1(DOT, IDENTIFIER),
            opt(preceded(AS, IDENTIFIER)),
        ),
    )
    .parse(input);
    fmap_iresult(parse_result, |(x, y)| Import {
        id: get_next_id(),
        path: x,
        alias: y,
        values: vec![],
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_imports() {
        check_match(
            "import foo.bar.baz",
            import,
            Import {
                id: 0,
                path: vec![
                    Identifier::from("foo"),
                    Identifier::from("bar"),
                    Identifier::from("baz"),
                ],
                alias: None,
                values: vec![],
            },
        );
    }
}
