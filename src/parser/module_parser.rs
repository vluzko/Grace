extern crate nom;
use self::nom::*;
use expression::*;
use parser::base::{ParserContext, Res};
use parser::parser_utils::iresult_helpers::*;
use parser::parser_utils::tokens::*;
use parser::parser_utils::*;
use parser::position_tracker::PosStr;
use std::collections::HashMap;

use general_utils::get_next_id;
use type_checking::types::Trait;

/// Parse a module.
pub fn module<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Node<Module>> {
    let mut context = ParserContext::empty();

    enum ModuleDec {
        Func(Node<Stmt>),
        Struct(Node<Stmt>),
        TraitDec(Trait),
        TraitImpl((Identifier, Identifier, Vec<Node<Stmt>>)),
    }

    let just_imports = preceded!(
        input,
        opt!(between_statement),
        many0c!(terminated!(import, between_statement))
    );

    let (remaining, imports) = match &just_imports {
        Ok((i, parsed_imports)) => {
            for import in parsed_imports {
                match &import.alias {
                    Some(ref alias) => context.imported.insert(alias.clone(), import.clone()),
                    None => context
                        .imported
                        .insert(import.path.get(0).unwrap().clone(), import.clone()),
                };
            }
            (i, parsed_imports)
        }
        _ => panic!(),
    };

    let w_self_type = ParserContext {
        imported: context.imported.clone(),
        can_use_self: true,
    };
    let declarations = terminated!(
        remaining,
        many1c!(terminated!(
            alt_complete!(
                map!(m!(context.function_declaration_stmt, 0), |x| {
                    ModuleDec::Func(x)
                }) | map!(m!(context.struct_declaration_stmt), |x| ModuleDec::Struct(
                    x
                )) | map!(m!(w_self_type.trait_parser), |x| ModuleDec::TraitDec(x))
                    | map!(m!(w_self_type.trait_impl), |x| ModuleDec::TraitImpl(x))
            ),
            between_statement
        )),
        alt_complete!(eof!() | EMPTY)
    );

    return fmap_node(declarations, |just_decs| {
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

        return Module {
            imports: imports.into_iter().map(|z| Box::new(z.clone())).collect(),
            functions: funcs,
            structs: structs,
            traits: traits,
            trait_implementations: trait_impls,
        };
    });
}

/// Parse an import statement.
pub(in parser) fn import<'a>(input: PosStr<'a>) -> Res<'a, Import> {
    let parse_result = preceded!(
        input,
        IMPORT,
        pair!(
            separated_nonempty_list_complete!(DOT, IDENTIFIER),
            optc!(preceded!(AS, IDENTIFIER))
        )
    );
    return fmap_iresult(parse_result, |(x, y)| Import {
        id: get_next_id(),
        path: x,
        alias: y,
        values: vec![],
    });
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
