pub trait Layer<T>{
    fn run_from_start(&[u8]) -> T;
}

pub struct Bytecode{}

impl Layer<String> for Bytecode {
    fn run_from_start(_input: &[u8]) -> String {
        panic!()
    }
}



pub struct Compilation {
    /// Will need to be a project / directory later.
    pub file: String,
    /// Counter for uniquely identifying every ASTNode.
    pub counter: i64
}

use std::sync::atomic::{AtomicUsize, Ordering};

static NODE_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);


pub fn get_next_id() -> u64 {
    let next_id = NODE_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    return next_id as u64;
}
