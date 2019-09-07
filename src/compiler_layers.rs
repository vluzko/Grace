use std::collections::HashMap;
use std::path::Path;
use std::fmt::Debug;
use std::io::prelude::*;
use std::fs::File;

use parser::Parseable;
use position_tracker::PosStr;
use scoping;
use scoping::Scoped;
use typing;
use typing::Typed;
use bytecode::ToBytecode;
use expression::Node;
use expression::Module;

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
    /// The path to the main file or folder.
    pub main_path: Option<Box<Path>>,
    /// All parsed modules.
    pub modules: HashMap<usize, Node<Module>>,
    /// Paths to all module files.
    pub module_paths: HashMap<usize, Box<Path>>,
    /// The dependency graph.
    pub dependencies: HashMap<usize, Vec<usize>>,
    /// File hashes
    pub hashes: HashMap<usize, u64>
}

impl Compilation {
    pub fn compile(file_name: String) {
        let mut f = File::open(file_name).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();
    }
}

pub fn compile_from_file(file_name: String) -> (Node<Module>, scoping::Context, HashMap<usize, typing::Type>, String){
    let mut f = File::open(file_name).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents).unwrap();
    return to_bytecode::<Node<Module>>(file_contents.as_bytes());
}

pub fn to_scopes<'a, T>(input: &'a [u8]) -> (T, scoping::Context)
where T: Parseable, T: Scoped<T> {
    let new_input = PosStr::from(input);
    let mut result = T::parse(new_input);
    let (id, mut init) = scoping::initial_context();
    let context = result.gen_scopes(id, &init);
    init.extend(context);
    return (result, init);
}

pub fn to_types<'a, T>(input: &'a [u8]) -> (T, scoping::Context, HashMap<usize, typing::Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T> {
    let (result, context): (T, scoping::Context) = to_scopes(input);
    let (type_map, _) = result.resolve_types(&context, HashMap::new());
    return (result, context, type_map);
}

pub fn to_type_rewrites<'a, T>(input: &'a [u8]) -> (T, scoping::Context, HashMap<usize, typing::Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: Debug {
    let (result, mut context, mut type_map): (T, scoping::Context, HashMap<usize, typing::Type>) = to_types(input);
    let rewritten = result.type_based_rewrite(&mut context, &mut type_map);
    return (rewritten, context, type_map);
}

pub fn to_bytecode<'a, T>(input: &'a [u8]) -> (T, scoping::Context, HashMap<usize, typing::Type>, String) 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: ToBytecode, T: Debug {
    let (result, context, mut type_map): (T, scoping::Context, HashMap<usize, typing::Type>) = to_type_rewrites(input);
    let bytecode = result.generate_bytecode(&context, &mut type_map);
    return (result, context, type_map, bytecode);
}


