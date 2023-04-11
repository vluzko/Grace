use super::*;
use testing::{minimal_examples, test_utils};

#[test]
fn type_check_function_def() {
    let context = Context::builtin();
    let (with_func, _) =
        test_utils::add_function_to_context(context, "foo", vec![Type::i32], vec![], Type::i32);
    let function_type = with_func.get_type(0, &Identifier::from("foo"));
    assert_eq!(
        function_type,
        Type::Function(
            vec![(Identifier::from("arg0"), Type::i32)],
            vec![],
            Box::new(Type::i32)
        )
    );
}

#[test]
fn type_check_assignment() {
    panic!()
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
    let context = minimal_examples::minimal_let_context();
    let stmt_type = context.get_type(0, &minimal_examples::minimal_identifier());
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
