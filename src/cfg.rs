extern crate petgraph;

use petgraph::{Graph, graph::NodeIndex};
use expression::{Identifier, Node, Module, Block, Stmt, Expr};
use scoping::Context2;
use typing::Type;

type Cfg = Graph::<CfgVertex, bool>;

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
    let empty = Graph::<CfgVertex, bool>::new();
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
    fn to_cfg(self, context: &Context2, current: Cfg) -> (Cfg, NodeIndex) {
        let mut new_cfg = current;
        let mut statements = vec!();
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
                    // the first statement of the child block is a branch
                    // statement--do the while loop or skip to the end
                    let branch = CfgStmt::Branch;
                    // make a node for the block the while loop is inside
                    let new_node = Node {
                        id: self.id,
                        scope: self.scope,
                        data: CfgVertex {
                            node_id: self.id,
                            statements: statements
                        }
                    };
                    let (mut block_cfg, block_nodeindex) = block.to_cfg(context, new_cfg);
                    // make an edge from the node to itself
                    block_cfg.add_edge(block_nodeindex, block_nodeindex, )
                    // recurse (and pass down the parent so edges
                    // from break statements know where to go)
                },
                _ => panic!()
            };
        }

        return new_cfg;
    }
}