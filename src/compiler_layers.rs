use std::collections::HashMap;
use std::fmt::Debug;

use expression;
use parser;
use parser::Parseable;
use scoping;
use scoping::Scoped;
use typing;
use typing::Typed;

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

pub fn parse(input: &[u8]) -> expression::Node<expression::Module>{
    panic!()
}

pub fn to_scopes<T>(input: &[u8]) -> (T, scoping::Context)
where T: Parseable, T: Scoped<T> {
    let mut result = T::parse(input);
    let context = result.gen_scopes2(0, &scoping::initial_context());
    return (result, context);
}

pub fn to_types<T>(input: &[u8]) -> (T, scoping::Context, HashMap<usize, typing::Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T> {
    let (result, context): (T, scoping::Context) = to_scopes(input);
    let (type_map, _) = result.resolve_types(&context, HashMap::new());
    return (result, context, type_map);
}

pub fn to_type_rewrites<T>(input: &[u8]) -> (T, scoping::Context, HashMap<usize, typing::Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: Debug {
    let (result, mut context, mut type_map): (T, scoping::Context, HashMap<usize, typing::Type>) = to_types(input);
    let rewritten = result.type_based_rewrite(&context, &mut type_map);
    return (rewritten, context, type_map);
}


