extern crate petgraph;

use petgraph::Graph;
use expression::{Identifier, Node, Module, Block, Stmt, Expr};
use scoping::Context2;
use typing::Type;

type Cfg = Graph::<CfgVertex, i64>;

pub struct CfgVertex {
    pub node_id: usize,
    pub statements: Vec<Node<CfgStmt>>
}

pub enum CfgStmt {
    Assignment  {name: Identifier, expression: Node<Expr>},
    Let         {name: Identifier, expression: Node<Expr>},
    Return      (Node<Expr>),
    Yield       (Node<Expr>),
    Branch      (Node<Expr>),
}

fn empty_cfg() -> Cfg {
    let empty = Graph::<CfgVertex, i64>::new();
    return empty;
}

/// Generate a control-flow graph.
// pub trait ToCfg {
//     fn to_cfg(self: Self, context: &Context2, current: Cfg) -> Cfg {
//         panic!()
//     }
// }

// impl ToCfg for Node<Module> {
//     fn to_cfg(self, context: &Context2, current: Cfg) -> Cfg {
//         panic!()
//     }
// }

impl Node<Block> {
    fn to_cfg(self, context: &Context2, current: Cfg) -> Cfg {
        let mut new_cfg = current;

        for stmt in self.data.statements.into_iter() {
            let val = match stmt.data {
                Stmt::LetStmt{name, expression, ..} => {
                    Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Let{name, expression}
                    }
                },
                Stmt::AssignmentStmt{name, expression} => {
                    Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Assignment{name, expression}
                    }
                },
                Stmt::ReturnStmt(val) => {
                    Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Return(val)
                    }
                },
                Stmt::YieldStmt(val) => {
                    Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Yield(val)
                    }
                },
                Stmt::WhileStmt{condition, block} => {
                    
                    panic!()
                },
                _ => panic!()
            };
        }

        return new_cfg;
    }
}

// impl ToCfg for Node<Stmt> {
//     fn to_cfg(self, context: &Context2, current: Cfg) -> Cfg {
//         return match &self.data {
//             Stmt::WhileStmt{ref condition, ref block} => {
//                 panic!()
//             },
//             _ => panic!()
//         };
//     }
// }

// impl ToCfg for Node<Expr> {
//     fn to_cfg(self, context: &Context2, current: Cfg) -> Cfg {
//         panic!()
//     }
// }