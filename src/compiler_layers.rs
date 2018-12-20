use ast_node::*;
use expression::*;
use std::collections::HashMap;

pub trait Layer<T>{
    fn run_from_start(&[u8]) -> T;
}

pub struct Bytecode{}

impl Layer<String> for Bytecode {
    fn run_from_start(input: &[u8]) -> String {
        panic!()
    }
}



pub struct Compilation {
    /// Will need to be a project / directory later.
    pub file: String,
    /// Counter for uniquely identifying every ASTNode.
    pub counter: i64
}


impl Compilation {

    fn parse(code: &[u8]) {

    }
}


fn compile(module: Module) {

}

use std::sync::atomic::{AtomicUsize, Ordering};
//
//pub trait Layer<T>{
//    fn run_from_start(&[u8]) -> T;
//}
//
//pub struct Bytecode{}
//
//impl Layer<String> for Bytecode {
//    fn run_from_start(input: &[u8]) -> String {
//        panic!()
//    }
//}
//
//
//
//pub struct Compilation {
//}
//
//impl Compilation{
//
//}


static NODE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn assign_node_ids(input: &ASTNode) -> (HashMap<ASTNode, i64>, HashMap<i64, ASTNode>) {
    let mut nodes_to_ids :HashMap<ASTNode, i64> = HashMap::new();
    let mut ids_to_nodes :HashMap<i64, ASTNode> = HashMap::new();
    let next_id = NODE_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    nodes_to_ids.insert(input, next_id);
    ids_to_nodes.insert(next_id, input);
}
