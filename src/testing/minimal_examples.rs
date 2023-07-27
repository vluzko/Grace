//! Helper functions to generate minimal examples of various datatypes
use std::collections::HashMap;

use expression;
use expression::{Block, Expr, Node, Stmt};
use testing::test_utils;
use type_checking::context::Context;
use type_checking::{context, types};

use crate::expression::Identifier;

/// Minimal block
pub fn minimal_block() -> Block {
    Block { statements: vec![] }
}

pub fn minimal_ret_block() -> Block {
    Block {
        statements: vec![Box::new(minimal_returnn())],
    }
}

pub fn minimal_unscoped_block() -> Node<Block> {
    let stmts = vec![
        Stmt::LetStmt {
            name: minimal_identifier(),
            type_annotation: None,
            expression: binary_op(),
        },
        Stmt::LetStmt {
            name: minimal_identifier(),
            type_annotation: None,
            expression: binary_op(),
        },
    ];
    Node::from(Block {
        statements: stmts.into_iter().map(|x| Box::new(Node::from(x))).collect(),
    })
}

pub fn minimal_op_block() -> Node<Block> {
    let stmts = vec![
        Stmt::LetStmt {
            name: minimal_identifier(),
            type_annotation: None,
            expression: binary_op_with_identifiers(),
        },
        Stmt::ReturnStmt(Node::from("x")),
    ];
    Node::from(Block {
        statements: stmts.into_iter().map(|x| Box::new(Node::from(x))).collect(),
    })
}

/// Minimal name
pub fn minimal_name() -> String {
    "x".to_string()
}

/// Minimal identifier
pub fn minimal_identifier() -> expression::Identifier {
    expression::Identifier::from(minimal_name())
}

pub fn minimal_identifiern() -> Node<Expr> {
    Node::from(Expr::from(minimal_identifier()))
}

pub fn minimal_identifier_2() -> Node<Expr> {
    Node::from("a")
}

pub fn minimal_identifier_3() -> Node<Expr> {
    Node::from("b")
}

/// Minimal generic expression
pub fn minimal_expression() -> Node<Expr> {
    Node::from(Expr::Int("1".to_string()))
}

/// Minimal boolean expression
pub fn minimal_bool_expression() -> Node<Expr> {
    Node::from(Expr::Bool(true))
}

pub fn minimal_int() -> Node<Expr> {
    Node::from(Expr::Int("1".to_string()))
}

pub fn binary_op() -> Node<Expr> {
    Node::from(Expr::BinaryExpr {
        left: Box::new(minimal_int()),
        right: Box::new(minimal_int()),
        operator: expression::BinaryOperator::Add,
    })
}

pub fn binary_op_with_identifiers() -> Node<Expr> {
    Node::from(Expr::BinaryExpr {
        left: Box::new(minimal_identifier_2()),
        right: Box::new(minimal_identifier_3()),
        operator: expression::BinaryOperator::Add,
    })
}

/// Minimal unary expression
pub fn minimal_unary() -> Node<Expr> {
    Node::from(Expr::UnaryExpr {
        operator: expression::UnaryOperator::Negative,
        operand: Box::new(minimal_int()),
    })
}

/// Minimal struct literal expression
pub fn minimal_struct_literal() -> Node<Expr> {
    Node::from(Expr::StructLiteral {
        base: Box::new(minimal_identifiern()),
        fields: vec![],
    })
}

/// Minimal attribute access
pub fn minimal_attribute_access() -> Node<Expr> {
    Node::from(Expr::AttributeAccess {
        base: Box::new(minimal_identifiern()),
        attribute: minimal_identifier(),
    })
}

/// Minimal vec literal with integer elements
pub fn vec_literal_numeric() -> Node<Expr> {
    Node::from(Expr::VecLiteral(vec![
        minimal_int(),
        minimal_int(),
        minimal_int(),
    ]))
}

/// Minimal tuple literal with integer elements
pub fn tuple_literal_numeric() -> Node<Expr> {
    Node::from(Expr::TupleLiteral(vec![
        minimal_int(),
        minimal_int(),
        minimal_int(),
    ]))
}

/// Minimal assignment statement
pub fn minimal_assn() -> Stmt {
    Stmt::AssignmentStmt {
        name: minimal_identifier(),
        expression: minimal_expression(),
    }
}

/// Minimal let statement
pub fn minimal_let() -> Stmt {
    Stmt::LetStmt {
        name: minimal_identifier(),
        type_annotation: None,
        expression: minimal_expression(),
    }
}

/// Minimal function declaration
pub fn minimal_function_dec() -> Stmt {
    Stmt::FunctionDecStmt {
        name: minimal_identifier(),
        args: vec![],
        kwargs: vec![],
        return_type: types::Type::i32,
        block: Node::from(minimal_ret_block()),
    }
}

/// Context containing the minimal function declaration
pub fn minimal_function_context() -> (Context, usize) {
    let context = context::Context::builtin();
    test_utils::add_function_to_context(
        context,
        minimal_name().as_str(),
        vec![],
        vec![],
        types::Type::i32,
    )
}

/// Minimal function call corresponding to minimal function declaration.
pub fn minimal_call() -> Node<Expr> {
    Node::from(Expr::FunctionCall {
        function: Box::new(minimal_identifiern()),
        args: vec![],
        kwargs: vec![],
    })
}

/// Function declaration with empty block
pub fn minimal_no_ret_function() -> Stmt {
    Stmt::FunctionDecStmt {
        name: minimal_identifier(),
        args: vec![],
        kwargs: vec![],
        return_type: types::Type::i32,
        block: Node::from(minimal_block()),
    }
}

/// Minimal if node
pub fn minimal_if() -> Stmt {
    Stmt::IfStmt {
        condition: minimal_bool_expression(),
        block: Node::from(minimal_block()),
        else_block: None,
    }
}

/// Minimal let node
pub fn minimal_letn() -> Node<Stmt> {
    Node::from(minimal_let())
}

/// Minimal assignment node
pub fn minimal_assnn() -> Node<Stmt> {
    Node::from(minimal_assn())
}

/// Minimal function declaration node
pub fn minimal_function_decn() -> Node<Stmt> {
    Node::from(minimal_function_dec())
}

pub fn minimal_function_with_args_and_ops() -> Node<Stmt> {
    let dec = Stmt::FunctionDecStmt {
        name: Identifier::from("add"),
        args: vec![
            (Identifier::from("a"), types::Type::i32),
            (Identifier::from("b"), types::Type::i32),
        ],
        kwargs: vec![],
        return_type: types::Type::i32,
        block: minimal_op_block(),
    };
    Node::from(dec)
}

/// Minimal function with no return node
pub fn minimal_no_ret_functionn() -> Node<Stmt> {
    Node::from(minimal_no_ret_function())
}

/// Minimal if node
pub fn minimal_ifn() -> Node<Stmt> {
    Node::from(minimal_if())
}

/// Minimal if with non-boolean condition node.
pub fn minimal_if_non_booln() -> Node<Stmt> {
    Node::from(Stmt::IfStmt {
        condition: minimal_expression(),
        block: Node::from(minimal_block()),
        else_block: None,
    })
}

/// Minimal if statement with non matching main and else blocks
pub fn minimal_if_nonmatching() -> Node<Stmt> {
    Node::from(Stmt::IfStmt {
        condition: minimal_bool_expression(),
        block: Node::from(minimal_block()),
        else_block: Some(Node::from(minimal_ret_block())),
    })
}

pub fn minimal_while() -> Node<Stmt> {
    Node::from(Stmt::WhileStmt {
        condition: minimal_bool_expression(),
        block: Node::from(minimal_block()),
    })
}

pub fn minimal_while_non_booln() -> Node<Stmt> {
    Node::from(Stmt::WhileStmt {
        condition: minimal_expression(),
        block: Node::from(minimal_block()),
    })
}

/// Minimal return node
pub fn minimal_returnn() -> Node<Stmt> {
    Node::from(Stmt::ReturnStmt(minimal_expression()))
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
pub fn trait_module() -> Node<expression::Module> {
    let t = minimal_trait();
    Node::from(expression::Module {
        functions: vec![],
        imports: vec![],
        structs: vec![],
        traits: HashMap::from([(t.name.clone(), t)]),
        trait_implementations: vec![],
    })
}

pub fn minimal_module() -> Node<expression::Module> {
    Node::from(expression::Module {
        functions: vec![Box::new(minimal_function_decn())],
        imports: vec![],
        structs: vec![],
        traits: HashMap::new(),
        trait_implementations: vec![],
    })
}

pub(crate) mod cfgs {
    use super::*;
    use cfg;
    use compiler_layers::UpToCfg;

    pub fn minimal_stmt() -> Node<cfg::CfgStmt> {
        Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    pub fn minimal_assn() -> Node<cfg::CfgStmt> {
        Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    pub fn minimal_cfg() -> (cfg::Cfg, Context) {
        let block = minimal_unscoped_block();
        let (_, cfg, context) = block.up_to_cfg().unwrap();
        (cfg, context)
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
    pub fn minimal_cfg_assn() -> Node<cfg::CfgStmt> {
        Node::from(cfg::CfgStmt::Assignment {
            name: minimal_identifier(),
            expression: minimal_bool_expression(),
        })
    }

    /// Minimal CFG return
    pub fn minimal_cfg_return() -> Node<cfg::CfgStmt> {
        Node::from(cfg::CfgStmt::Return(minimal_bool_expression()))
    }
}
