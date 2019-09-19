use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::fmt::Debug;
use std::io::prelude::*;
use std::fs::File;

use parser::Parseable;
use position_tracker::PosStr;
// use scoping;
use scoping::{
    Scoped,
    Context,
    initial_context
};
// use typing;
use typing::{
    Type,
    Typed
};
use bytecode::ToBytecode;
use expression::{
    Node,
    Module,
    Identifier
};
use general_utils::{
    extend_map,
    get_next_module_id
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledModule {
    pub ast: Node<Module>,
    pub scope: Context,
    pub type_map: HashMap<usize, Type>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compilation {
    /// The path to the main file or folder.
    pub main_path: Option<Box<Path>>,
    /// All parsed modules.
    pub modules: HashMap<usize, CompiledModule>,
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

    /// Compile an individual module.
    pub fn compile_tree(&self, file_name: &Box<Path>) -> (usize, Compilation) {
        let mut f = File::open(file_name).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();
        let mut tree = Compilation::empty();
        let mut dependencies = vec!();
        let mut parsed_module = <Node<Module> as Parseable>::parse(PosStr::from(file_contents.as_bytes()));
        for import in &parsed_module.data.imports {
            let path = module_path_to_path(&import.data.path);
            let (dep_id, subtree) = self.compile_tree(&path);
            dependencies.push(dep_id);
            tree = tree.merge(subtree);
        }
        // Get the scopes for the module.
        let (id, mut init) = initial_context();
        let context = parsed_module.gen_scopes(id, &init);
        init.extend(context);
        // Get the types for the module and do type_rewrites.
        let (mut type_map, _) = parsed_module.resolve_types(&init, HashMap::new());
        let rewritten = parsed_module.type_based_rewrite(&mut init, &mut type_map);
        // Put the results in the tree.
        let compiled = CompiledModule {
            ast: rewritten,
            scope: init,
            type_map: type_map
        };
        let id = get_next_module_id();
        tree.modules.insert(id, compiled);
        tree.module_paths.insert(id, file_name.clone());
        tree.dependencies.insert(id, dependencies);
        return (id, tree);
    }

    pub fn merge(mut self, other: Compilation) -> Compilation {
        self.modules = extend_map(self.modules, other.modules);
        self.module_paths = extend_map(self.module_paths, other.module_paths);
        self.dependencies = extend_map(self.dependencies, other.dependencies);
        self.hashes = extend_map(self.hashes, other.hashes);
        return self;
    }

    pub fn empty() -> Compilation {
        return Compilation {
            main_path: None,
            modules: HashMap::new(),
            module_paths: HashMap::new(),
            dependencies: HashMap::new(),
            hashes: HashMap::new()
        };
    }
}

fn module_path_to_path(module_path: &Vec<Identifier>) -> Box<Path> {
    let mut path = PathBuf::new();
    for component in module_path {
        path.push(component.name.as_str());
    }
    return path.into_boxed_path();
}

pub fn compile_from_file(file_name: String) -> (Node<Module>, Context, HashMap<usize, Type>, String){
    let mut f = File::open(file_name).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents).unwrap();
    return to_bytecode::<Node<Module>>(file_contents.as_bytes());
}

pub fn to_scopes<'a, T>(input: &'a [u8]) -> (T, Context)
where T: Parseable, T: Scoped<T> {
    let new_input = PosStr::from(input);
    let mut result = T::parse(new_input);
    let (id, mut init) = initial_context();
    let context = result.gen_scopes(id, &init);
    init.extend(context);
    return (result, init);
}

pub fn to_types<'a, T>(input: &'a [u8]) -> (T, Context, HashMap<usize, Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T> {
    let (result, context): (T, Context) = to_scopes(input);
    let (type_map, _) = result.resolve_types(&context, HashMap::new());
    return (result, context, type_map);
}

pub fn to_type_rewrites<'a, T>(input: &'a [u8]) -> (T, Context, HashMap<usize, Type>) 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: Debug {
    let (result, mut context, mut type_map): (T, Context, HashMap<usize, Type>) = to_types(input);
    let rewritten = result.type_based_rewrite(&mut context, &mut type_map);
    return (rewritten, context, type_map);
}

pub fn to_bytecode<'a, T>(input: &'a [u8]) -> (T, Context, HashMap<usize, Type>, String) 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: ToBytecode, T: Debug {
    let (result, context, mut type_map): (T, Context, HashMap<usize, Type>) = to_type_rewrites(input);
    let bytecode = result.generate_bytecode(&context, &mut type_map);
    return (result, context, type_map, bytecode);
}


