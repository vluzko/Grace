use super::*;
use testing::{minimal_examples, test_utils};
use type_checking::type_check::GetContext;

#[test]
fn type_check_function_def() {
    let context = Context::builtin();
    let mut stmt = minimal_examples::minimal_function_decn();
    let scoped_context = stmt.set_scope(context.root_id, context);
    let (typed_context, ret_type) = stmt.add_to_context(scoped_context).unwrap();
    let function_type = typed_context.get_type(0, &minimal_examples::minimal_identifier());
    assert_eq!(function_type, minimal_examples::minimal_function_type());
    assert_eq!(function_type, ret_type);
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
    let stmt_type = typed_context.get_type(
        typed_context.root_id,
        &minimal_examples::minimal_identifier(),
    );
    assert_eq!(stmt_type, Type::i32);
}

#[test]
#[should_panic]
fn assignment_no_init() {
    let context = Context::empty();
    let mut stmt = minimal_examples::minimal_assnn();
    stmt.scope = context.root_id;
    let (typed_context, _) = stmt.add_to_context(context).unwrap();
    let stmt_type = typed_context.get_type(
        typed_context.root_id,
        &minimal_examples::minimal_identifier(),
    );
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
    let stmt_type = typed_context.get_type(
        typed_context.root_id,
        &minimal_examples::minimal_identifier(),
    );
    assert_eq!(stmt_type, Type::i32);
}

#[test]
fn type_check_if_stmt() {
    panic!()
}

#[test]
fn type_check_while() {
    panic!()
}
