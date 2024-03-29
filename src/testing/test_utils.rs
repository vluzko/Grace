use std::collections::BTreeMap;

use expression::*;
use type_checking::context::Context;
use type_checking::scope::SetScope;
use type_checking::type_check::GetContext;
use type_checking::types::Type;

/// Add a function of the specified type and name to the given context.
pub(crate) fn add_function_to_context(
    mut context: Context,
    func_name_str: &str,
    arg_types: Vec<Type>,
    kwarg_types: Vec<Type>,
    ret_type: Type,
) -> (Context, usize) {
    let func_name = Identifier::from(func_name_str);
    let args: Vec<(Identifier, Type)> = arg_types
        .into_iter()
        .enumerate()
        .map(|(i, x)| (Identifier::from(format!("arg{}", i)), x))
        .collect();
    let kwargs: Vec<(Identifier, Type)> = kwarg_types
        .into_iter()
        .enumerate()
        .map(|(i, x)| (Identifier::from(format!("kwarg{}", i)), x))
        .collect();
    let function_type = Type::Function(args.clone(), kwargs, Box::new(ret_type.clone()));
    let null_function = wrap(Stmt::FunctionDecStmt {
        name: func_name.clone(),
        args,
        kwargs: vec![],
        block: Node::from(Block { statements: vec![] }),
        return_type: ret_type,
    });
    context.append_declaration(0, &func_name, &null_function);
    context.add_type(null_function.id, function_type);
    (context, null_function.id)
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
        fields,
    });

    // Add type to context
    let record_type = Type::Record(struct_t.keys().cloned().collect(), struct_t);
    context.define_type(name.clone(), record_type);
    context.add_type(struct_dec.id, Type::Named(name.clone()));

    // Add it to the context
    let top_scope = context.get_mut_scope(context.root_id).unwrap();
    top_scope.append_declaration(&name, &Box::new(struct_dec));

    context
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
    let scoped_context = stmt.set_scope(0, context);
    let (mut new_c, _) = stmt.add_to_context(scoped_context).unwrap();
    new_c.append_declaration(0, &Identifier::from(ident_name), &Box::new(stmt));

    new_c
}
