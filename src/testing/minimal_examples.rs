//! Helper functions to generate minimal examples of various datatypes
use std::collections::HashMap;

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
pub fn minimal_bool_expression() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::Bool(true))
}

pub fn minimal_int() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::Int("1".to_string()))
}

/// Minimal expression node
pub fn minimal_node_expression() -> expression::Node<expression::Expr> {
    expression::Node::from(minimal_expression())
}

/// Minimal vec literal
pub fn minimal_vec_literal() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::VecLiteral(
        vec![minimal_node_expression()],
    ))
}

/// Minimal vec literal with integer elements
pub fn vec_literal_numeric() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::VecLiteral(vec![
        minimal_int(),
        minimal_int(),
        minimal_int(),
    ]))
}

/// Minimal set literal with integer elements
pub fn set_literal_numeric() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::SetLiteral(vec![
        minimal_int(),
        minimal_int(),
        minimal_int(),
    ]))
}

/// Minimal tuple literal with integer elements
pub fn tuple_literal_numeric() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::TupleLiteral(vec![
        minimal_int(),
        minimal_int(),
        minimal_int(),
    ]))
}

/// Minimal map literal with integer elements
pub fn map_literal_numeric() -> expression::Node<expression::Expr> {
    expression::Node::from(expression::Expr::MapLiteral(vec![
        (minimal_int(), minimal_int()),
        (minimal_int(), minimal_int()),
    ]))
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
        condition: minimal_bool_expression(),
        block: expression::Node::from(minimal_block()),
        else_block: None,
    }
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

/// Minimal if statement with non matching main and else blocks
pub fn minimal_if_nonmatching() -> expression::Node<expression::Stmt> {
    expression::Node::from(expression::Stmt::IfStmt {
        condition: minimal_bool_expression(),
        block: expression::Node::from(minimal_block()),
        else_block: Some(expression::Node::from(minimal_ret_block())),
    })
}

pub fn minimal_while() -> expression::Node<expression::Stmt> {
    expression::Node::from(expression::Stmt::WhileStmt {
        condition: minimal_bool_expression(),
        block: expression::Node::from(minimal_block()),
    })
}

pub fn minimal_while_non_booln() -> expression::Node<expression::Stmt> {
    expression::Node::from(expression::Stmt::WhileStmt {
        condition: expression::Node::from(minimal_expression()),
        block: expression::Node::from(minimal_block()),
    })
}

/// Minimal return node
pub fn minimal_returnn() -> expression::Node<expression::Stmt> {
    expression::Node::from(expression::Stmt::ReturnStmt(minimal_node_expression()))
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

/// A trait with a single function.
pub fn minimal_trait() -> types::Trait {
    let functions = HashMap::from([(
        minimal_identifier(),
        types::Type::Function(vec![], vec![], Box::new(types::Type::i32)),
    )]);
    types::Trait {
        name: minimal_identifier(),
        functions,
    }
}

/// Module containing a trait.
pub fn trait_module() -> expression::Node<expression::Module> {
    let t = minimal_trait();
    expression::Node::from(expression::Module {
        functions: vec![],
        imports: vec![],
        structs: vec![],
        traits: HashMap::from([(t.name.clone(), t)]),
        trait_implementations: vec![],
    })
}

pub(crate) mod cfgs {
    use super::*;
    use cfg;

    pub fn minimal_stmt() -> expression::Node<cfg::CfgStmt> {
        expression::Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    pub fn minimal_assn() -> expression::Node<cfg::CfgStmt> {
        expression::Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    /// Minimal block vertex.
    pub fn minimal_block() -> cfg::CfgVertex {
        cfg::CfgVertex::Block(vec![minimal_stmt(), minimal_assn()])
    }

    /// Minimal if start vertex.
    pub fn minimal_if_start() -> cfg::CfgVertex {
        cfg::CfgVertex::IfStart(minimal_bool_expression(), types::Type::i32)
    }

    /// Minimal while loop start vertex
    pub fn minimal_loop_start() -> cfg::CfgVertex {
        cfg::CfgVertex::LoopStart(minimal_bool_expression())
    }

    /// Minimal break vertex
    pub fn minimal_break() -> cfg::CfgVertex {
        cfg::CfgVertex::Break(vec![minimal_stmt(), minimal_assn()])
    }

    /// Minimal continue vertex
    pub fn minimal_continue() -> cfg::CfgVertex {
        cfg::CfgVertex::Continue(vec![minimal_stmt(), minimal_assn()])
    }

    /// Minimal else vertex
    pub fn minimal_else() -> cfg::CfgVertex {
        cfg::CfgVertex::Else
    }

    /// Minimal end vertex
    pub fn minimal_end() -> cfg::CfgVertex {
        cfg::CfgVertex::End(2)
    }

    /// Minimal entry vertex
    pub fn minimal_entry() -> cfg::CfgVertex {
        cfg::CfgVertex::Entry
    }

    /// Minimal exit vertex
    pub fn minimal_exit() -> cfg::CfgVertex {
        cfg::CfgVertex::Exit
    }

    /// Minimal CFG assignment
    pub fn minimal_cfg_assn() -> expression::Node<cfg::CfgStmt> {
        expression::Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    /// Minimal CFG return
    pub fn minimal_cfg_return() -> expression::Node<cfg::CfgStmt> {
        expression::Node::from(cfg::CfgStmt::Return(minimal_bool_expression()))
    }
}
