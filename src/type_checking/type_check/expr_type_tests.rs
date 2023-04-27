use type_checking::type_check::*;
// use super::*;
use grace_error::ErrorDetails;
use testing::test_utils;
use type_checking::types::Trait;

use crate::testing::minimal_examples;

/// Check that an expression has the desired type.
fn check_expr(context: Context, mut expr: Node<Expr>, expected_type: Type) {
    let w_scope = expr.set_scope(context.root_id, context);
    let (_, t) = expr.add_to_context(w_scope).unwrap();
    assert_eq!(t, expected_type);
}

/// Check that an expression has the desired type in a trivial context
fn simple_check_expr(expr: Node<Expr>, expected_type: Type) {
    let context = Context::builtin();
    check_expr(context, expr, expected_type);
}

/// Check that type checking *fails*
fn fail_check_expr(context: Context, mut expr: Node<Expr>) {
    let res = expr.add_to_context(context);
    match res {
        Ok((_, t)) => panic!("Expected failed type check. Expr has type {:?}", t),
        Err(e) => match e.underlying {
            ErrorDetails::TypeError { .. } => {}
            x => panic!("Got non-type error: {:?}", x),
        },
    }
}

#[test]
fn test_literals() {
    simple_check_expr(Node::<Expr>::from(5), Type::i32);
    simple_check_expr(Node::<Expr>::from(5.0), Type::f32);
    simple_check_expr(Node::<Expr>::from(true), Type::boolean);
}

#[test]
fn type_check_function_call() {
    // Create function type
    let context = Context::builtin();
    let (with_func, _) =
        test_utils::add_function_to_context(context, "foo", vec![Type::i32], vec![], Type::i32);
    let expr = Node::from(Expr::FunctionCall {
        function: wrap(Expr::from("foo")),
        args: vec![Node::from(1)],
        kwargs: vec![],
    });

    check_expr(with_func, expr, Type::i32);
}

#[test]
fn function_call_wrong_args() {
    let context = Context::builtin();
    let (with_func, _) =
        test_utils::add_function_to_context(context, "foo", vec![Type::i32], vec![], Type::i32);
    let expr = Node::from(Expr::FunctionCall {
        function: wrap(Expr::from("foo")),
        args: vec![Node::from(true)],
        kwargs: vec![],
    });
    fail_check_expr(with_func, expr);
}

#[test]
fn type_check_comparison_exprs() {
    let operand = Node::from(0);
    let comparisons = vec![BinaryOperator::Equal, BinaryOperator::Unequal];

    for comp in comparisons {
        let expr = Node::from(Expr::BinaryExpr {
            operator: comp,
            left: Box::new(operand.clone()),
            right: Box::new(operand.clone()),
        });
        simple_check_expr(expr, Type::boolean);
    }
}

#[test]
fn type_check_unary_exprs() {
    let operand = Node::from(true);
    let operators = vec![UnaryOperator::Not];
    for op in operators {
        let expr = Node::from(Expr::UnaryExpr {
            operator: op,
            operand: Box::new(operand.clone()),
        });
        simple_check_expr(expr, Type::boolean);
    }
}

#[test]
fn type_check_unary_exprs_expect_fail() {
    let context = Context::builtin();
    let operand = Node::from(7);
    let operator = UnaryOperator::Not;
    let expr = Node::from(Expr::UnaryExpr {
        operator: operator,
        operand: Box::new(operand.clone()),
    });
    fail_check_expr(context, expr);
}

#[test]
fn type_check_binary_exprs() {
    let operand = Node::from(0);
    let comparisons = vec![BinaryOperator::Equal, BinaryOperator::Unequal];

    for comp in comparisons {
        let expr = Node::from(Expr::BinaryExpr {
            operator: comp,
            left: Box::new(operand.clone()),
            right: Box::new(operand.clone()),
        });
        simple_check_expr(expr, Type::boolean);
    }
}

#[test]
fn type_check_complex_literals() {
    // panic!("To implement: Tests of complex literals type checking: vector, set, tuple")
}

#[test]
fn type_check_struct_literals() {
    let context = Context::builtin();
    let mut attr_map = BTreeMap::new();
    attr_map.insert(Identifier::from("a"), Type::i32);

    let new_context = test_utils::add_struct_to_context(context, "A", attr_map.clone());

    let expr = Node::from(Expr::StructLiteral {
        base: Box::new(Node::from("A")),
        fields: vec![Node::from(4)],
    });

    check_expr(new_context, expr, Type::Named(Identifier::from("A")));
}

#[test]
fn type_check_struct_literal_fail() {
    let context = Context::builtin();
    let mut attr_map = BTreeMap::new();
    attr_map.insert(Identifier::from("a"), Type::i32);

    let new_context = test_utils::add_struct_to_context(context, "A", attr_map);

    let expr = Node::from(Expr::StructLiteral {
        base: Box::new(Node::from("A")),
        fields: vec![Node::from(true)],
    });

    fail_check_expr(new_context, expr);
}

#[test]
fn type_check_attribute_access() {
    let context = Context::builtin();
    let mut attr_map = BTreeMap::new();
    attr_map.insert(Identifier::from("a"), Type::i32);

    let new_context = test_utils::add_struct_to_context(context, "A", attr_map);

    let base = Node::from(Expr::StructLiteral {
        base: Box::new(Node::from("A")),
        fields: vec![Node::from(4)],
    });
    let expr = Node::from(Expr::AttributeAccess {
        base: Box::new(base.clone()),
        attribute: Identifier::from("a"),
    });
    check_expr(new_context, expr, Type::i32);
}

#[test]
fn type_check_nonexistent_attribute_access() {
    let context = Context::builtin();
    let base = Node::from(0);
    let attribute = Identifier::from("string");
    let expr = Node::from(Expr::AttributeAccess {
        base: Box::new(base.clone()),
        attribute: attribute,
    });
    fail_check_expr(context, expr);
}

#[test]
fn type_check_trait_access() {
    let mut context = Context::builtin();
    // make a trait
    let trait_functions = hashmap! {Identifier::from("test_funcname")=>Type::Function(vec!(), vec!(), Box::new(Type::i32))};
    let test_trait = Trait {
        name: Identifier::from("test_trait"),
        functions: trait_functions.clone(),
    };
    context
        .traits
        .insert(Identifier::from("test_trait"), test_trait);
    // make a struct
    let test_struct_map = btreemap! {Identifier::from("test_identifier")=>Type::i32};
    let mut test_context =
        test_utils::add_struct_to_context(context, "test_struct", test_struct_map);
    // implement that trait for that struct
    let trait_impl_key = (
        Identifier::from("test_trait"),
        Type::Named(Identifier::from("test_struct")),
    );
    test_context
        .trait_implementations
        .insert(trait_impl_key, trait_functions);
    // call it, expect OK;
    let base = Node::from(Expr::StructLiteral {
        base: Box::new(Node::from("test_struct")),
        fields: vec![Node::from(4)],
    });
    let expr = Node::from(Expr::AttributeAccess {
        base: Box::new(base.clone()),
        attribute: Identifier::from("test_funcname"),
    });
    check_expr(
        test_context,
        expr,
        Type::Function(vec![], vec![], Box::new(Type::i32)),
    );
}

#[test]
fn type_check_module_access() {
    let mut context = Context::builtin();

    // Add module type to context
    let func_type = Type::func_no_args(Type::i32);
    let type_map = btreemap! {Identifier::from("a") => func_type.clone()};
    let module_type = Type::module_from_map(type_map);
    let id = general_utils::get_next_id();
    context.add_type(id, module_type);

    let module_access = Node::from(Expr::ModuleAccess(
        id,
        vec![Identifier::from("A"), Identifier::from("a")],
    ));

    check_expr(context, module_access, func_type);
}

#[test]
fn type_check_module_access_fail() {
    let mut context = Context::builtin();

    // Add module type to context
    let func_type = Type::func_no_args(Type::i32);
    let type_map = btreemap! {Identifier::from("a") => func_type.clone()};
    let module_type = Type::module_from_map(type_map);
    let id = general_utils::get_next_id();
    context.add_type(id, module_type);

    let module_access = Node::from(Expr::ModuleAccess(
        id,
        vec![Identifier::from("A"), Identifier::from("b")],
    ));

    fail_check_expr(context, module_access);
}

#[test]
fn type_check_method_call() {
    panic!()
}

#[test]
fn type_check_identifier() {
    let init = Context::builtin();
    let context = test_utils::add_identifier_to_context(init, "a", Expr::from(0));
    let expr = Node::from("a");
    check_expr(context, expr, Type::i32);
}

#[test]
fn type_check_vec_literal() {
    let expr = minimal_examples::vec_literal_numeric();
    let expected = Type::Parameterized(Identifier::from("Vector"), vec![Type::i32]);
    simple_check_expr(expr, expected);
}

#[test]
fn type_check_set_literal() {
    let expr = minimal_examples::set_literal_numeric();
    let expected = Type::Parameterized(Identifier::from("Set"), vec![Type::i32]);
    simple_check_expr(expr, expected);
}

#[test]
fn type_check_tuple_literal() {
    let expr = minimal_examples::tuple_literal_numeric();
    simple_check_expr(expr, Type::Product(vec![Type::i32, Type::i32, Type::i32]));
}

#[test]
fn type_check_map_literal() {
    let expr = minimal_examples::map_literal_numeric();
    let expected = Type::Parameterized(Identifier::from("Map"), vec![Type::i32, Type::i32]);
    simple_check_expr(expr, expected);
}

#[cfg(test)]
mod expected_failures {
    use compiler_layers;
    #[test]
    #[should_panic]
    fn add_incompatible() {
        let input = "fn a():\n   let x = \"a\" + 0";
        compiler_layers::Compilation::compile_from_string(&input.to_string());
    }
}
