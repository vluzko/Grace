/// Control-flow graph for optimizations.
use std::collections::HashMap;

extern crate petgraph;

use petgraph::{Graph, graph::NodeIndex, graph::EdgeIndex};

use expression::{Identifier, Node, Module, Block, Stmt, Expr};
use type_checking::context::Context;
use type_checking::types::Type;

pub type CfgMap = HashMap<Identifier, Cfg>;

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
    IfStart(Node<Expr>, Type),
    Else,
    Break(Vec<Node<CfgStmt>>),
    Continue(Vec<Node<CfgStmt>>),
    End(usize),
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
    pub fn add_block(&mut self, statements: Vec<Node<CfgStmt>>, previous: Option<NodeIndex>) -> NodeIndex {
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
pub fn module_to_cfg(module: &Node<Module>, context: &Context) -> CfgMap {
    let mut cfg_map = HashMap::new();
    for decl in &module.data.functions {
        match decl.data {
            Stmt::FunctionDecStmt{ref name, ref block, ..} => {
                let cfg = block_to_cfg(block, context, Cfg::empty(), None).0;
                cfg_map.insert(name.clone(), cfg);
            },
            _ => panic!()
        }
    }
    for decl in &module.data.structs {
        match decl.data {
            Stmt::StructDec{..} => {

            },
            _ => panic!()
        }
    }
    for (trait_name, struct_name, function_decs) in &module.data.trait_implementations {
        for decl in function_decs {
            match decl.data {
                Stmt::FunctionDecStmt{ref name, ref block, ..} => {
                    let cfg = block_to_cfg(block, context, Cfg::empty(), None).0;
                    cfg_map.insert(Identifier::from(format!("{}.{}.{}", trait_name, struct_name, name)), cfg);
                },
                Stmt::StructDec{..} => {},
                _ => panic!()
            }
        }
    }
    return cfg_map;
}


#[allow(unused_assignments)]
/// Add the contents of a block to a CFG.
/// 
/// # Arguments
/// 
/// * `context` - 
/// * `current` - 
/// * `loop_start` -
fn block_to_cfg(block: &Node<Block>, context: &Context, current: Cfg, loop_start: Option<NodeIndex>) -> (Cfg, NodeIndex, Vec<NodeIndex>) {
    let mut new_cfg = current;
    // The set of statements in the current CFG block.
    let mut statements = vec!();
    let mut need_edge_to_next_block = vec!();
    let mut entry_index = None;
    let mut previous_index = None;

    for stmt in &block.data.statements {
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
                let new_index = new_cfg.add_block(statements, previous_index);

                // Create a vertex containing just the while loop condition.
                // The loop body passes back to here, as do all continue statements.
                let condition_index = new_cfg.add_node(CfgVertex::LoopStart(condition.clone()));
                new_cfg.add_edge(new_index, condition_index, true);

                // Add the while loop block to the CFG.
                let res = block_to_cfg(block, context, new_cfg, Some(condition_index));
                new_cfg = res.0;

                // The condition block passes inside the loop when it evaluates to true.
                new_cfg.add_edge(condition_index, res.1, true);

                // Breaks *within* the while loop go immediately after the while loop.
                // Note that this structure is *not* the same as `need_edge_to_next_block`, which need an edge
                // to the loop *containing* the current statement.
                // e.g. if we're looking at while loop A inside while loop B, the former breaks out of A and
                // the latter breaks out of B.
                let mut need_edge_to_next_block = res.2;
                need_edge_to_next_block.push(condition_index);

                // We create an empty vertex to serve as a placeholder for the next vertex.
                // All break statements in the while loop and the while loop exit have an edge to it.
                let empty_index = new_cfg.add_node(CfgVertex::End(2));
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
                let new_index = new_cfg.add_block(statements, previous_index);
                let stmt_type = context.get_node_type(stmt.id);

                // A block for the initial if condition.
                let condition_index = new_cfg.add_node(CfgVertex::IfStart(condition.clone(), stmt_type));
                // Attach the condition to the previous block.
                new_cfg.add_edge(new_index, condition_index, false);

                // Add the initial if block to the CFG.
                let mut res = block_to_cfg(block, context, new_cfg, loop_start);
                new_cfg = res.0;
                // Add an edge from the if condition to the inner block.
                new_cfg.add_edge(condition_index, res.1, true);
                // Track any breaks contained in the inner block.
                need_edge_to_next_block.append(&mut res.2);

                let end_index = new_cfg.add_node(CfgVertex::End(3));
                
                match else_block {
                    Some(b) => {
                        let else_index = new_cfg.add_node(CfgVertex::Else);
                        
                        // Add the else block to the CFG.
                        let mut else_res = block_to_cfg(b, context, new_cfg, loop_start);
                        new_cfg = else_res.0;
                        
                        need_edge_to_next_block.append(&mut else_res.2);

                        // Add an edge from the last condition to the else block.
                        new_cfg.add_edge(else_index, else_res.1, false);
                        new_cfg.add_edge(condition_index, else_index, false);
                        // Add an edge from the else block to the end.
                        new_cfg.add_edge(else_res.1, end_index, false);
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
    let final_index = new_cfg.add_block(statements, previous_index);
    entry_index = update_entry(entry_index, &final_index);

    return (new_cfg, entry_index.unwrap(), need_edge_to_next_block);
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
