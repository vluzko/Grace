use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::fmt::Debug;
use std::io::prelude::*;
use std::fs::File;

extern crate itertools;

use parser::Parseable;
use position_tracker::PosStr;
// use scoping;
use scoping::{
    Scoped,
    Context,
    CanModifyScope,
    initial_context,
    base_scope
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
    get_next_id,
    get_next_scope_id,
    get_next_module_id
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledModule {
    pub id: usize,
    pub ast: Node<Module>,
    pub context: Context,
    pub type_map: HashMap<usize, Type>,
    // The path to the module file.
    pub path: Box<Path>,
    // The dependencies of this module.
    pub dependencies: Vec<usize>,
    // The MD5 hash of the *file* describing the module.
    pub hash: u64
}

impl CompiledModule {
    fn full_name(&self) -> String {
        let joined = self.path.components().into_iter();
        panic!()
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compilation {
    /// The path to the main file or folder.
    pub main_path: Option<Box<Path>>,
    /// All parsed modules.
    pub modules: HashMap<usize, CompiledModule>
}

impl Compilation {
    pub fn compile(file_name: String) {
        let mut f = File::open(file_name).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();
    }

    /// Compile the module tree rooted at the given file name.
    pub fn compile_tree(&self, file_name: &Box<Path>) -> (usize, Compilation) {
        let mut f = File::open(file_name).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        // Parse the module
        let mut parsed_module = <Node<Module> as Parseable>::parse(PosStr::from(file_contents.as_bytes()));

        // Set everything up for compiling the dependencies.
        let mut tree = Compilation::empty();
        let mut dependencies = vec!();
        let mut init_scope = base_scope();
        let mut init_type_map = HashMap::new();

        // Compile dependencies
        for import in &parsed_module.data.imports {
            let path = module_path_to_path(&import.data.path);
            let (sub_id, sub_tree) = self.compile_tree(&path);
            let sub_module = sub_tree.modules.get(&sub_id).unwrap();

            let submodule_name = itertools::join(import.data.path.iter().map(|x| x.name.clone()), ".");

            // Add the imported functions to scope and types.
            for func_dec in &sub_module.ast.data.declarations {
                let full_name = Identifier{name: format!("{}.{}", submodule_name, func_dec.data.get_name())};
                let import_id = get_next_id();

                // Add the imported function to the initial scope.
                init_scope.declarations.insert(full_name.clone(), CanModifyScope::Import(import_id));
                init_scope.declaration_order.insert(full_name, init_scope.declaration_order.len());

                // Add the imported function type to the starting type map.
                let func_type = sub_module.type_map.get(&func_dec.id).unwrap();
                init_type_map.insert(import_id, func_type.clone());
            }

            dependencies.push(sub_id);
            tree = tree.merge(sub_tree);
        }

        // Create the initial context for the current module.
        let mut scopes = HashMap::new();
        let scope_id = get_next_scope_id();
        scopes.insert(scope_id, init_scope);
        let mut init_context = Context {
            scopes: scopes,
            containing_scopes: HashMap::new()
        };

        // Calculate all scopes for the current module.
        let context = parsed_module.gen_scopes(scope_id, &init_context);
        init_context.extend(context);

        // Get the types for the module and do type_rewrites.
        let (mut type_map, _) = parsed_module.resolve_types(&init_context, init_type_map);
        let rewritten = parsed_module.type_based_rewrite(&mut init_context, &mut type_map);

        // Put the results in the tree.
        let module_id = get_next_module_id();
        let compiled = CompiledModule {
            id: module_id,
            ast: rewritten,
            context: init_context,
            type_map: type_map,
            path: file_name.clone(),
            dependencies: dependencies,
            hash: 0
        };
        tree.modules.insert(module_id, compiled);
        return (module_id, tree);
    }
    
    /// Merge two Compilations together.
    pub fn merge(mut self, other: Compilation) -> Compilation {
        self.modules = extend_map(self.modules, other.modules);
        return self;
    }

    /// Create an empty Compilation.
    pub fn empty() -> Compilation {
        return Compilation {
            main_path: None,
            modules: HashMap::new(),
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


