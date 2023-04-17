//! Helper functions to generate minimal examples of various datatypes
use expression;
use testing::test_utils;
use type_checking::{context, types};

pub fn minimal_name() -> String {
    "x".to_string()
}

pub fn minimal_identifier() -> expression::Identifier {
    expression::Identifier::from(minimal_name())
}

pub fn minimal_expression() -> expression::Expr {
    expression::Expr::Int("1".to_string())
}

pub fn minimal_node_expression() -> expression::Node<expression::Expr> {
    expression::Node::from(minimal_expression())
}

pub fn minimal_let() -> expression::Stmt {
    expression::Stmt::LetStmt {
        name: minimal_identifier(),
        type_annotation: None,
        expression: minimal_node_expression(),
    }
}

pub fn minimal_letn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_let())
}

pub fn minimal_struct_type() -> types::Type {
    let mut attr_map = std::collections::BTreeMap::new();
    attr_map.insert(minimal_identifier(), types::Type::i32);
    types::Type::Record(vec![minimal_identifier()], attr_map)
}

pub fn minimal_let_context() -> context::Context {
    let context = context::Context::builtin();
    test_utils::add_identifier_to_context(context, minimal_name().as_str(), minimal_expression())
}

pub fn minimal_struct_context() -> context::Context {
    let context = context::Context::builtin();
    let mut attr_map = std::collections::BTreeMap::new();
    attr_map.insert(minimal_identifier(), types::Type::i32);
    test_utils::add_struct_to_context(context, minimal_name().as_str(), attr_map)
}
