use std::collections::HashMap;
use std::collections::BTreeMap;
use std::path::Path;
use std::path::PathBuf;
use std::fmt::Debug;
use std::io::prelude::*;
use std::fs::{File, canonicalize, create_dir_all};
use std::env;

extern crate itertools;

use parser::Parseable;
use position_tracker::PosStr;
// use scoping;
use scoping::{
    Scoped,
    Scope,
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
    Identifier,
    Import
};
use general_utils::extend_map;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledModule {
    pub ast: Node<Module>,
    pub context: Context,
    pub type_map: HashMap<usize, Type>,
    // The path to the module file.
    pub path: Box<Path>,
    // The dependencies of this module.
    pub dependencies: Vec<String>,
    // The MD5 hash of the *file* describing the module.
    pub hash: u64
}

impl CompiledModule {
    pub fn get_type(&self) -> Type {
        let mut attribute_map = BTreeMap::new();
        for func_dec in &self.ast.data.declarations {
            let func_type = self.type_map.get(&func_dec.id).unwrap().clone();
            attribute_map.insert(func_dec.data.get_name(), func_type);
        }

        return Type::Record(attribute_map);
    }

    pub fn get_internal_module_name(idents: &Vec<Identifier>) -> String {
        return itertools::join(idents.iter().map(|x| x.name.clone()), ".");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compilation {
    /// The path to the main file or folder.
    pub main_path: Option<Box<Path>>,
    /// All parsed modules.
    pub modules: HashMap<String, CompiledModule>,
    pub root_name: Option<String>
}

impl Compilation {
    pub fn compile(file_name: String) -> Compilation {
        let original_path = env::current_dir();
        let path = Path::new(&file_name);
        let absolute_path = canonicalize(path).unwrap().into_boxed_path();
        match env::set_current_dir(path.parent().unwrap()) {
            Ok(_) => {},
            Err(x) => panic!("{:?}", x)
        };
        let boxed = Box::from(Path::new(path.file_name().unwrap()));
        let mut compilation = Compilation{
            main_path: Some(absolute_path),
            modules: HashMap::new(),
            root_name: Some(path_to_module_reference(&boxed))
        };
        compilation.compile_tree(&boxed);
        match env::set_current_dir(original_path.unwrap()) {
            Ok(_) => {},
            Err(x) => panic!("{:?}", x)
        }
        return compilation;
    }

    /// Get the full record type of a submodule.
    /// a.b.c turns into Record(a => Record(b => Record(c => functions_in_c)))
    fn get_submodule_type(sub_module: &CompiledModule, import: &Import) -> Type {

        // Add the types of all the imported functions to the record type.
        let mut record_type = BTreeMap::new();
        for func_dec in &sub_module.ast.data.declarations {
            let func_type = sub_module.type_map.get(&func_dec.id).unwrap().clone();
            record_type.insert(func_dec.data.get_name(), func_type);
        }

        // The type of the full module (including all the parent modules).
        let module_type = Type::flatten_to_record(&import.path, record_type);

        return module_type;
    }

    fn add_submodule_to_scope(import: &Import, mut init_scope: Scope) -> Scope {
        // Add the imported module to scope.
        // TODO: I think this can just be in scoping.
        let module_root = import.path.get(0).unwrap().clone();
        init_scope.declarations.insert(module_root.clone(), CanModifyScope::ImportedModule(import.id));
        init_scope.declaration_order.insert(module_root, init_scope.declaration_order.len());

        return init_scope;
    }

    /// Compile the module tree rooted at the given file name.
    pub fn compile_tree(&mut self, file_name: &Box<Path>) {
        let mut f = File::open(file_name).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        // Parse the module
        let mut parsed_module = <Node<Module> as Parseable>::parse(PosStr::from(file_contents.as_bytes()));
        let module_name = path_to_module_reference(&file_name);

        // Set everything up for compiling the dependencies.
        let mut new_imports = vec!();
        let mut dependencies = vec!();
        let mut init_scope = base_scope();
        let mut init_type_map = HashMap::new();

        // Compile dependencies
        for import in &parsed_module.data.imports {
            let path = module_path_to_path(&import.path);
            let submodule_name = itertools::join(import.path.iter().map(|x| x.name.clone()), ".");

            let submodule = match self.modules.get(&submodule_name) {
                Some(x) => x,
                None => {
                    self.compile_tree(&path);
                    self.modules.get(&submodule_name).unwrap()
                }
            };

            // A vector containing all the names of the imported functions.
            let exports = submodule.ast.data.declarations.iter().map(|x| x.data.get_name().clone()).collect();

            // Add the imported functions to scope.
            init_scope = Compilation::add_submodule_to_scope(import, init_scope);

            // The type of the full module (including all the parent modules).
            let module_type = Compilation::get_submodule_type(submodule, import);
            init_type_map.insert(import.id, module_type);

            new_imports.push(Box::new(Import {
                id: import.id,
                path: import.path.clone(),
                alias: import.alias.clone(),
                values: exports
            }));
            dependencies.push(submodule_name);
        }

        parsed_module.data.imports = new_imports;

        // Create the initial context for the current module.
        let mut init_context = Context::empty();
        let scope_id = init_context.new_scope(init_scope);

        // Calculate all scopes for the current module.
        let context = parsed_module.gen_scopes(scope_id, &init_context);
        init_context.extend(context);

        // Get the types for the module and do type_rewrites.
        let (mut type_map, _) = parsed_module.resolve_types(&init_context, init_type_map);
        let rewritten = parsed_module.type_based_rewrite(&mut init_context, &mut type_map);

        // Put the results in the tree.
        let compiled = CompiledModule {
            ast: rewritten,
            context: init_context,
            type_map: type_map,
            path: file_name.clone(),
            dependencies: dependencies,
            hash: 0
        };
        self.modules.insert(module_name, compiled);
    }
    
    // Generate bytecode for all compiled modules, and output the result to files. Return the output of the main file, if it exists.
    pub fn generate_wast_files(&self, output_dir: &Box<Path>) -> Option<String> {
        let mut ret_str = None;
        for (k, v) in self.modules.iter() {
            let path_str = k.replace(".", "/");
            let relative_path = &Path::new(&path_str);
            let bytecode = v.ast.generate_bytecode(&v.context, &mut v.type_map.clone());
            let mut output_path = output_dir.join(relative_path);
            output_path.set_extension("wat");
            match create_dir_all(output_path.parent().unwrap()) {
                Ok(_) => {},
                Err(x) => panic!("{:?}", x)
            };
            let outfile = File::create(output_path);
            outfile.unwrap().write_all(bytecode.as_bytes()).unwrap();
            if Some(k) == self.root_name.as_ref() {
                ret_str = Some(bytecode);
            }
        }
        return ret_str;
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
            root_name: None,
            modules: HashMap::new(),
        };
    }
}

fn module_path_to_path(module_path: &Vec<Identifier>) -> Box<Path> {
    let mut path = PathBuf::new();
    for component in module_path {
        path.push(component.name.as_str());
    }
    return match path.is_dir() {
        true => path.into_boxed_path(),
        false => {
            path.set_extension("gr");
            path.into_boxed_path()
        }
    };
}

/// Convert a file path to an internal module reference.
fn path_to_module_reference(path: &Box<Path>) -> String {
    // WOW it's hard to convert Paths to Strings.
    let without_extension = path.parent().unwrap().join(path.file_stem().unwrap());
    return itertools::join(without_extension.components().map(|x| x.as_os_str().to_os_string().into_string().unwrap()), ".");
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_to_string;
  
    #[test]
    fn simple_imports_test() {
        let file_path = "./test_data/simple_imports_test/file_1.gr".to_string();
        let compiled = Compilation::compile(file_path);
        // println!("{:?}", compiled);
        assert_eq!(compiled.modules.len(), 3);
    }

    #[test]
    fn simple_imports_compile_test() {
        let file_path = "./test_data/simple_imports_test/file_1.gr".to_string();
        let compiled = Compilation::compile(file_path);
        let outpath = Path::new("./test_data/outputs/simple_imports_test/");
        let _ = compiled.generate_wast_files(&Box::from(outpath));
        for i in (1..4).rev() {
            let actual_file = format!("./test_data/outputs/simple_imports_test/file_{}.wat", i);
            let expected_file = format!("./test_data/outputs/simple_imports_test/file_{}_expected.wat", i);
            let actual = read_to_string(actual_file).unwrap();
            let expected = read_to_string(expected_file).unwrap();
            assert_eq!(actual, expected);
        }
    }
}
