extern crate petgraph;

use petgraph::*;
use expression::{Assignment, Identifier, Node};
use typing::Type;

pub struct CfgVertex {
    pub node_id: usize,
    pub statements: Vec<Node<CfgStmt>>
}

pub enum CfgStmt {
    AssignmentStmt  {name: Identifier, expression: Node<Expr>},
    LetStmt         {name: Identifier, expression: Node<Expr>},
    ReturnStmt      (Node<Expr>),
    YieldStmt       (Node<Expr>),
    BranchStmt      (Node<Expr>),
}