//! Helper functions to generate minimal examples of various datatypes
use expression;
use testing::test_utils;
use type_checking::{context, types};

/// Minimal block
pub fn minimal_block() -> expression::Block {
    return expression::Block { statements: vec![] };
}

pub fn minimal_ret_block() -> expression::Block {
    return expression::Block {
        statements: vec![Box::new(minimal_returnn())],
    };
}

/// Minimal name
pub fn minimal_name() -> String {
    "x".to_string()
}

/// Minimal identifier
pub fn minimal_identifier() -> expression::Identifier {
    expression::Identifier::from(minimal_name())
}

/// Minimal generic expression
pub fn minimal_expression() -> expression::Expr {
    expression::Expr::Int("1".to_string())
}

/// Minimal boolean expression
pub fn minimal_bool_expression() -> expression::Expr {
    expression::Expr::Bool(true)
}

/// Minimal expression node
pub fn minimal_node_expression() -> expression::Node<expression::Expr> {
    expression::Node::from(minimal_expression())
}

/// Minimal assignment statement
pub fn minimal_assn() -> expression::Stmt {
    expression::Stmt::AssignmentStmt {
        name: minimal_identifier(),
        expression: minimal_node_expression(),
    }
}

/// Minimal let statement
pub fn minimal_let() -> expression::Stmt {
    expression::Stmt::LetStmt {
        name: minimal_identifier(),
        type_annotation: None,
        expression: minimal_node_expression(),
    }
}

/// Minimal function declaration
pub fn minimal_function_dec() -> expression::Stmt {
    expression::Stmt::FunctionDecStmt {
        name: minimal_identifier(),
        args: vec![],
        kwargs: vec![],
        return_type: types::Type::i32,
        block: expression::Node::from(minimal_ret_block()),
    }
}

/// Function declaration with empty block
pub fn minimal_no_ret_function() -> expression::Stmt {
    expression::Stmt::FunctionDecStmt {
        name: minimal_identifier(),
        args: vec![],
        kwargs: vec![],
        return_type: types::Type::i32,
        block: expression::Node::from(minimal_block()),
    }
}

/// Minimal if node
pub fn minimal_if() -> expression::Stmt {
    expression::Stmt::IfStmt {
        condition: expression::Node::from(minimal_bool_expression()),
        block: expression::Node::from(minimal_block()),
        else_block: None,
    }
}

/// Minimal return statement
pub fn minimal_return() -> expression::Stmt {
    expression::Stmt::ReturnStmt(minimal_node_expression())
}

/// Minimal let node
pub fn minimal_letn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_let())
}

/// Minimal assignment node
pub fn minimal_assnn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_assn())
}

/// Minimal function declaration node
pub fn minimal_function_decn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_function_dec())
}

/// Minimal function with no return node
pub fn minimal_no_ret_functionn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_no_ret_function())
}

/// Minimal if node
pub fn minimal_ifn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_if())
}

/// Minimal if with non-boolean condition node.
pub fn minimal_if_non_booln() -> expression::Node<expression::Stmt> {
    expression::Node::from(expression::Stmt::IfStmt {
        condition: minimal_node_expression(),
        block: expression::Node::from(minimal_block()),
        else_block: None,
    })
}

/// Minimal return node
pub fn minimal_returnn() -> expression::Node<expression::Stmt> {
    expression::Node::from(minimal_return())
}

/// Minimal function type
pub fn minimal_function_type() -> types::Type {
    types::Type::Function(vec![], vec![], Box::new(types::Type::i32))
}

/// Minimal struct type
pub fn minimal_struct_type() -> types::Type {
    let mut attr_map = std::collections::BTreeMap::new();
    attr_map.insert(minimal_identifier(), types::Type::i32);
    types::Type::Record(vec![minimal_identifier()], attr_map)
}

/// Minimal context with minimal struct
pub fn minimal_struct_context() -> context::Context {
    let context = context::Context::builtin();
    let mut attr_map = std::collections::BTreeMap::new();
    attr_map.insert(minimal_identifier(), types::Type::i32);
    test_utils::add_struct_to_context(context, minimal_name().as_str(), attr_map)
}
