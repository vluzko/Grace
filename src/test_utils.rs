use std::collections::{BTreeMap};

use expression::*;
use type_checking::context::Context;
use type_checking::type_check::GetContext;
use type_checking::types::Type;


/// Add a function of the specified type and name to the given context.
pub(crate) fn add_function_to_context(
    mut context: Context,
    func_name_str: &str,
    arg_types: Vec<Type>,
    ret_type: Type,
) -> (Context, usize) {
    let func_name = Identifier::from(func_name_str);
    let args: Vec<(Identifier, Type)> = arg_types
        .into_iter()
        .enumerate()
        .map(|(i, x)| (Identifier::from(format!("arg{}", i)), x))
        .collect();
    let function_type = Type::Function(args.clone(), Box::new(ret_type.clone()));
    let null_function = wrap(Stmt::FunctionDecStmt {
        name: func_name.clone(),
        args: args,
        kwargs: vec![],
        block: Node::from(Block { statements: vec![] }),
        return_type: ret_type,
    });
    context.append_declaration(0, &func_name, &null_function);
    context.add_type(null_function.id, function_type);
    return (context, null_function.id);
}

/// Add a struct of the specified type and name to the context.
/// Used only for testing.
pub(crate) fn add_struct_to_context(
    mut context: Context,
    struct_name: &str,
    struct_t: BTreeMap<Identifier, Type>,
) -> Context {
    let name = Identifier::from(struct_name);
    let fields = struct_t.clone().into_iter().collect();
    // Create struct declaration
    let struct_dec = Node::from(Stmt::StructDec {
        name: name.clone(),
        fields: fields,
    });

    // Add type to context
    let record_type = Type::Record(struct_t.keys().cloned().collect(), struct_t);
    context.define_type(name.clone(), record_type.clone());
    context.add_type(struct_dec.id, record_type);

    // Add it to the context
    let top_scope = context.get_mut_scope(context.root_id);
    top_scope.append_declaration(&name, &Box::new(struct_dec));

    return context;
}

pub(crate) fn add_identifier_to_context(
    context: Context,
    ident_name: &str,
    ident_value: Expr,
) -> Context {
    let mut stmt = Node::from(Stmt::LetStmt {
        name: Identifier::from(ident_name),
        type_annotation: None,
        expression: Node::from(ident_value),
    });
    let (mut new_c, _) = stmt.scopes_and_types(0, context).unwrap();
    new_c.append_declaration(0, &Identifier::from(ident_name), &Box::new(stmt));

    return new_c;
}