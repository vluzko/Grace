/// Control-flow graph for optimizations.
use std::collections::HashMap;

extern crate petgraph;

use petgraph::{Graph, graph::NodeIndex, graph::EdgeIndex};

use expression::{Identifier, Node, Module, Block, Stmt, Expr};
use general_utils::get_next_id;
use scoping::Context;

#[derive(Debug, Clone)]
pub struct Cfg {
    pub entry_index: Option<NodeIndex>,
    pub graph: Graph::<CfgVertex, bool>
}

/// A vertex of the Cfg. Represents some kind of block or control flow structure.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgVertex {
    Entry,
    Block(Vec<Node<CfgStmt>>),
    LoopStart(Node<Expr>),
    IfStart(Node<Expr>),
    Else,
    Break(Vec<Node<CfgStmt>>),
    Continue(Vec<Node<CfgStmt>>),
    End,
    Exit
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgStmt {
    Assignment  {name: Identifier, expression: Node<Expr>},
    Let         {name: Identifier, expression: Node<Expr>},
    Return      (Node<Expr>),
    Yield       (Node<Expr>),
    Branch      (Node<Expr>),
}

impl Cfg {
    pub fn empty() -> Self {
        return Cfg {
            entry_index: None,
            graph: Graph::<CfgVertex, bool>::new()
        };
    }

    pub fn add_node(&mut self, node: CfgVertex) -> NodeIndex {
        let index = self.graph.add_node(node);
        match self.entry_index {
            Some(_) => {},
            None => {self.entry_index = Some(index);}
        }
        return index;
    }

    pub fn add_edge(&mut self, a: NodeIndex, b: NodeIndex, w: bool) -> EdgeIndex {
        return self.graph.add_edge(a, b, w);
    }

    /// Add a node containing the given statements.
    pub fn add_block(&mut self, node_id: usize, statements: Vec<Node<CfgStmt>>, previous: Option<NodeIndex>) -> NodeIndex {
        let new_node = CfgVertex::Block(statements);
        let new_index = self.add_node(new_node);
        match previous {
            Some(x) => {self.add_edge(x, new_index, false);},
            None => {}
        };

        return new_index;
    }

}

/// Add the contents of a module to a CFG
pub fn module_to_cfg(module: &Node<Module>, context: &Context) -> HashMap<Identifier, Cfg> {
    let mut cfg_map = HashMap::new();
    for decl in &module.data.declarations {
        match decl.data {
            Stmt::FunctionDecStmt{ref name, ref block, ..} => {
                let cfg = block.to_cfg(context, Cfg::empty(), None).0;
                cfg_map.insert(name.clone(), cfg);
            },
            Stmt::StructDec{..} => {},
            _ => panic!()
        }
    }
    return cfg_map;
}


impl Node<Block> {
    /// Add the contents of a block to a CFG.
    /// 
    /// # Arguments
    /// 
    /// * `context` - 
    /// * `current` - 
    /// * `loop_start` - 
    fn to_cfg(&self, context: &Context, current: Cfg, loop_start: Option<NodeIndex>) -> (Cfg, NodeIndex, Vec<NodeIndex>) {
        let mut new_cfg = current;
        // The set of statements in the current CFG block.
        let mut statements = vec!();
        let mut need_edge_to_next_block = vec!();
        let mut entry_index = None;
        let mut previous_index = None;

        for stmt in &self.data.statements {
            match stmt.data {
                Stmt::LetStmt{ref name, ref expression, ..} => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Let{name: name.clone(), expression: expression.clone()}
                    });
                },
                Stmt::AssignmentStmt{ref name, ref expression} => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope,
                        data: CfgStmt::Assignment{name: name.clone(), expression: expression.clone()}
                    });
                },
                Stmt::ReturnStmt(ref val) => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Return(val.clone())
                    });
                },
                Stmt::YieldStmt(ref val) => {
                    statements.push(Node {
                        id: stmt.id,
                        scope: stmt.scope, 
                        data: CfgStmt::Yield(val.clone())
                    });
                },
                Stmt::WhileStmt{ref condition, ref block} => {
                    // Collect the current batch of statements into a block and add it to the CFG.
                    let new_index = new_cfg.add_block(self.id, statements, previous_index);

                    // Create a vertex containing just the while loop condition.
                    // The loop body passes back to here, as do all continue statements.
                    let condition_index = new_cfg.add_node(CfgVertex::LoopStart(condition.clone()));
                    new_cfg.add_edge(new_index, condition_index, true);
                    
                    // Add the while loop block to the CFG.
                    let res = block.to_cfg(context, new_cfg, Some(condition_index));
                    new_cfg = res.0;

                    // The condition block passes inside the loop when it evaluates to true.
                    new_cfg.add_edge(condition_index, res.1, true);

                    // Breaks *within* the while loop go immediately after the while loop.
                    // Note that this structure is *not* the same as `need_edge_to_next_block`, which need an edge
                    // to the loop *containing* the current statement.
                    // e.g. if we're looking at while loop A inside while loop B, the former breaks out of A and
                    // the latter breaks out of B.
                    let mut need_edge_to_loop_end = res.2;
                    need_edge_to_loop_end.push(condition_index);

                    // We create an empty vertex to serve as a placeholder for the next vertex.
                    // All break statements in the while loop and the while loop exit have an edge to it.
                    let empty_index = new_cfg.add_node(CfgVertex::End);
                    new_cfg = add_edges_to_next(new_cfg, need_edge_to_next_block, empty_index);

                    // Reset for the next iteration.
                    need_edge_to_next_block = vec!();
                    previous_index = Some(empty_index);
                    statements = vec!();
                },
                Stmt::BreakStmt => {
                    let new_node = CfgVertex::Break(statements);
                    let new_index = new_cfg.add_node(new_node);
                    match previous_index {
                        Some(i) => {new_cfg.add_edge(i, new_index, false);},
                        None => {}
                    };
                    
                    need_edge_to_next_block.push(new_index.clone());
                    previous_index = Some(new_index);

                    statements = vec!();
                    break;
                },
                Stmt::ContinueStmt => {
                    let new_index = new_cfg.add_node(CfgVertex::Continue(statements));

                    match previous_index {
                        Some(i) => {new_cfg.add_edge(i, new_index, false);},
                        None => {}
                    };

                    // Add an edge from the new block back to the loop start.
                    match loop_start {
                        Some(i) => new_cfg.add_edge(new_index, i, false),
                        None => panic!()
                    };

                    previous_index = Some(new_index);
                    statements = vec!();
                    break;
                },
                Stmt::IfStmt{ref condition, ref block, ref else_block} => {
                    // Collect existing statements into a block.
                    let new_index = new_cfg.add_block(self.id, statements, previous_index);

                    // A block for the initial if condition.
                    let mut condition_index = new_cfg.add_node(CfgVertex::IfStart(condition.clone()));
                    // Attach the condition to the previous block.
                    new_cfg.add_edge(new_index, condition_index, false);

                    // Add the initial if block to the CFG.
                    let mut res = block.to_cfg(context, new_cfg, loop_start);
                    new_cfg = res.0;
                    // Add an edge from the if condition to the inner block.
                    new_cfg.add_edge(condition_index, res.1, true);
                    // Track any breaks contained in the inner block.
                    need_edge_to_next_block.append(&mut res.2);

                    let end_index = new_cfg.add_node(CfgVertex::End);

                    match else_block {
                        Some(b) => {
                            
                            // Add the else block to the CFG.
                            let mut res = b.to_cfg(context, new_cfg, loop_start);
                            new_cfg = res.0;
                            
                            need_edge_to_next_block.append(&mut res.2);

                            // Add an edge from the last condition to the else block.
                            new_cfg.add_edge(condition_index, res.1, false);
                            // Add an edge from the else block to the end.
                            new_cfg.add_edge(res.1, end_index, false);
                        },
                        None => {
                            new_cfg.add_edge(condition_index, end_index, false);
                            new_cfg.add_edge(res.1, end_index, false);
                        }
                    };

                    previous_index = Some(end_index);

                    statements = vec!()

                },
                Stmt::PassStmt => {},
                _ => panic!()
            };
        }

        // Collect any leftover statements into a block.
        let final_index = new_cfg.add_block(get_next_id(), statements, previous_index);
        entry_index = update_entry(entry_index, &final_index);

        return (new_cfg, entry_index.unwrap(), need_edge_to_next_block);
    }
}

fn update_entry(current: Option<NodeIndex>, new: &NodeIndex) -> Option<NodeIndex> {
    return match current {
        Some(x) => Some(x), 
        None => Some(new.clone())
    };
}

fn add_edges_to_next(mut current: Cfg, sources: Vec<NodeIndex>, target: NodeIndex) -> Cfg {
    for source in sources {
        current.add_edge(source, target, false);
    }
    return current;
}

#[cfg(test)]
mod tests {

}
