use super::*;
use testing::minimal_examples;
use type_checking::type_check::GetContext;

#[test]
fn type_check_function_def() {
    let context = Context::builtin();
    let mut stmt = minimal_examples::minimal_function_decn();
    let scoped_context = stmt.set_scope(context.root_id, context);
    let (typed_context, ret_type) = stmt.add_to_context(scoped_context).unwrap();
    let function_type = typed_context
        .get_type(0, &minimal_examples::minimal_identifier())
        .unwrap();
    assert_eq!(function_type, minimal_examples::minimal_function_type());
    assert_eq!(function_type, ret_type);
}

#[test]
#[should_panic]
fn function_def_no_return_statement() {
    let context = Context::builtin();
    let mut stmt = minimal_examples::minimal_no_ret_functionn();
    let scoped_context = stmt.set_scope(context.root_id, context);
    let (typed_context, ret_type) = stmt.add_to_context(scoped_context).unwrap();
    println!("{:?}", typed_context.all_names_and_types());
    // let function_type = typed_context.get_type(0, &minimal_examples::minimal_identifier());
    // assert_eq!(function_type, minimal_examples::minimal_function_type());
    // assert_eq!(function_type, ret_type);
}

#[test]
fn type_check_assignment() {
    let context = Context::empty();
    let mut let_s = minimal_examples::minimal_letn();
    let mut stmt = minimal_examples::minimal_assnn();
    let_s.scope = context.root_id;
    stmt.scope = context.root_id;
    let (typed_context, _) = let_s.add_to_context(context).unwrap();
    let (typed_context, _) = stmt.add_to_context(typed_context).unwrap();
    let stmt_type = typed_context
        .get_type(
            typed_context.root_id,
            &minimal_examples::minimal_identifier(),
        )
        .unwrap();
    assert_eq!(stmt_type, Type::i32);
}

#[test]
#[should_panic]
fn assignment_no_init() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_assnn();
    stmt.scope = context.root_id;
    let (typed_context, _) = stmt.add_to_context(context).unwrap();
    let stmt_type = typed_context
        .get_type(
            typed_context.root_id,
            &minimal_examples::minimal_identifier(),
        )
        .unwrap();
    assert_eq!(stmt_type, Type::i32);
}

#[test]
fn type_check_struct_dec() {
    let context = minimal_examples::minimal_struct_context();
    let struct_type = context
        .get_defined_type(&minimal_examples::minimal_identifier())
        .unwrap();
    assert_eq!(struct_type, minimal_examples::minimal_struct_type())
}

#[test]
fn type_check_let_stmt() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_letn();
    stmt.scope = context.root_id;
    let (typed_context, _) = stmt.add_to_context(context).unwrap();
    let stmt_type = typed_context
        .get_type(
            typed_context.root_id,
            &minimal_examples::minimal_identifier(),
        )
        .unwrap();
    assert_eq!(stmt_type, Type::i32);
}

#[test]
fn type_check_if_statement() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_ifn();
    stmt.scope = context.root_id;
    let (typed_context, t) = stmt.add_to_context(context).unwrap();
    let found_t = typed_context.get_node_type(stmt.id).unwrap();
    assert_eq!(found_t, t);
    assert_eq!(found_t, Type::empty);
}

#[test]
#[should_panic(expected = "Non boolean condition: i32")]
fn if_stmt_non_boolean() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_if_non_booln();
    stmt.scope = context.root_id;
    stmt.add_to_context(context).unwrap();
}

#[test]
#[should_panic(expected = "Type error. Tried to merge empty and i32")]
fn if_stmt_mismatched_blocks() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_if_nonmatching();
    let w_scopes = stmt.set_scope(context.root_id, context);
    stmt.add_to_context(w_scopes).unwrap();
}

#[test]
fn type_check_while() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_while();
    let w_scopes = stmt.set_scope(context.root_id, context);
    let (context, t) = stmt.add_to_context(w_scopes).unwrap();
    assert_eq!(t, Type::empty);
    assert_eq!(context.get_node_type(stmt.id).unwrap(), Type::empty);
}

#[test]
#[should_panic(expected = "Non boolean condition: i32")]
fn while_stmt_non_boolean() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_while_non_booln();
    let w_scopes = stmt.set_scope(context.root_id, context);
    stmt.add_to_context(w_scopes).unwrap();
}
