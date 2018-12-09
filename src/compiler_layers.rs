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

