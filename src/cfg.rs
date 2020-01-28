/// Control-flow graph for optimizations.


extern crate petgraph;

use petgraph::{Graph, graph::NodeIndex};
use expression::{Identifier, Node, Module, Block, Stmt, Expr};
use general_utils::get_next_id;
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

impl Node<Module> {

}

impl Node<Block> {
    /// Add the contents of a block to a CFG.
    /// 
    /// # Arguments
    /// 
    /// * `context` - 
    /// * `current` - 
    /// * `loop_start` - 
    fn to_cfg(self, context: &Context2, current: Cfg, loop_start: Option<NodeIndex>) -> (Cfg, Vec<NodeIndex>) {
        let mut new_cfg = current;
        // The set of statements in the current CFG block.
        let mut statements = vec!();
        let mut need_edge_to_next_block = vec!();

        for stmt in self.data.statements.into_iter() {
            match stmt.data {
                Stmt::LetStmt{name, expression, ..} => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Let{name, expression}
                    });
                },
                Stmt::AssignmentStmt{name, expression} => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Assignment{name, expression}
                    });
                },
                Stmt::ReturnStmt(val) => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Return(val)
                    });
                },
                Stmt::YieldStmt(val) => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Yield(val)
                    });
                },
                Stmt::WhileStmt{condition, block} => {
                    // the first statement of the child block is a branch
                    // statement--do the while loop or skip to the end
                    // make a node for the block the while loop is inside
                    let new_node = CfgVertex {
                        node_id: self.id,
                        statements: statements
                    };
                    
                    let new_index = new_cfg.add_node(new_node);

                    new_cfg = add_edges_to_next(new_cfg, need_edge_to_next_block, new_index);
                    need_edge_to_next_block = vec!();

                    // Create a vertex containing just the while loop condition.
                    let condition_stmt = Node {
                        id: get_next_id(),
                        scope: self.scope,
                        data: CfgStmt::Branch(condition)
                    };

                    let condition_node = CfgVertex {
                        node_id: condition_stmt.id,
                        statements: vec!(condition_stmt)
                    };

                    let condition_index = new_cfg.add_node(condition_node);
                    new_cfg.add_edge(new_index, condition_index, true);
                    
                    let res = block.to_cfg(context, new_cfg, Some(condition_index));
                    new_cfg = res.0;
                    need_edge_to_next_block = res.1;
                    need_edge_to_next_block.push(condition_index);
                    statements = vec!();
                },
                Stmt::BreakStmt => {
                    let new_node = CfgVertex {
                        node_id: self.id,
                        statements: statements
                    };
                    let new_index = new_cfg.add_node(new_node);
                    new_cfg = add_edges_to_next(new_cfg, need_edge_to_next_block, new_index);
                    need_edge_to_next_block = vec!();

                    break;
                },
                Stmt::ContinueStmt => {
                    let new_node = CfgVertex {
                        node_id: self.id,
                        statements: statements
                    };
                    let new_index = new_cfg.add_node(new_node);
                    new_cfg = add_edges_to_next(new_cfg, need_edge_to_next_block, new_index);
                    need_edge_to_next_block = vec!();

                    // Add an edge from the new block back to the loop start.
                    match loop_start {
                        Some(i) => new_cfg.add_edge(new_index, i, false),
                        None => panic!()
                    };
                    break;
                },
                Stmt::IfStmt{condition, block, elifs, else_block} => {
                    let new_node = CfgVertex {
                        node_id: self.id,
                        statements: statements
                    };
                    let new_index = new_cfg.add_node(new_node);

                    new_cfg = add_edges_to_next(new_cfg, need_edge_to_next_block, new_index);

                    // Create a vertex containing just the while loop condition.

                    let condition_node = CfgVertex {
                        node_id: get_next_id(),
                        statements: vec!(Node {
                            id: get_next_id(),
                            scope: self.scope,
                            data: CfgStmt::Branch(condition)
                        })
                    };

                    let mut condition_index = new_cfg.add_node(condition_node);
                    new_cfg.add_edge(new_index, condition_index, true);

                    let res = block.to_cfg(context, new_cfg, Some(condition_index));
                    new_cfg = res.0;
                    need_edge_to_next_block = res.1;

                    for (elif_cond, elif_block) in elifs {
                        let elif_cond_index = new_cfg.add_node(CfgVertex {
                            node_id: get_next_id(),
                            statements: vec!(Node {
                                id: get_next_id(),
                                scope: self.scope,
                                data: CfgStmt::Branch(elif_cond)
                            })
                        });

                        new_cfg.add_edge(condition_index, elif_cond_index, false);
                        condition_index = elif_cond_index;

                        let res = elif_block.to_cfg(context, new_cfg, Some(elif_cond_index));
                        new_cfg = res.0;
                        need_edge_to_next_block = res.1;
                    }

                    match else_block {
                        Some(b) => {
                            
                        },
                        None => {}
                    }

                    statements = vec!()

                },
                Stmt::PassStmt => {},
                _ => panic!()
            };
        }

        return (new_cfg, need_edge_to_next_block);
    }
}

fn add_edges_to_next(mut current: Cfg, sources: Vec<NodeIndex>, target: NodeIndex) -> Cfg {
    for source in sources {
        current.add_edge(source, target, false);
    }
    return current;
}

// prior_stuff -> condition_block -> first_while_block, follower

// condition

// block
// branch(condition)
// -> then_block, else_block
// if condition
// then
// code
// break -> follower
// else
// other_block
// block -> next_block

// continue -> condition

// block

// end_block