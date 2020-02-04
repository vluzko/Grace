use std::collections::HashMap;
use std::collections::BTreeMap;
use std::path::Path;
use std::path::PathBuf;
use std::fmt::Debug;
use std::io::prelude::*;
use std::fs::{File, canonicalize, create_dir_all};

extern crate itertools;

use parser::Parseable;
use position_tracker::PosStr;
// use scoping;
use scoping::{
    Scope,
    Context,
    Context2,
    CanModifyScope,
    GetContext,
    initial_context,
    builtin_context,
    base_scope
};
// use typing;
use typing::{
    Type,
    Typed
};
// use bytecode::ToBytecode;
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
    pub context: Context2,
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
        let mut attribute_order = vec!();
        for func_dec in &self.ast.data.declarations {
            let func_type = self.context.type_map.get(&func_dec.id).unwrap().clone();
            attribute_order.push(func_dec.data.get_name());
            attribute_map.insert(func_dec.data.get_name(), func_type);
        }

        return Type::Record(attribute_order, attribute_map);
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
        let path = Path::new(&file_name);
        let absolute_path = canonicalize(path).unwrap().into_boxed_path();

        // Panics if the file_name ends in ".."
        let boxed = Box::from(Path::new(path.file_name().unwrap()));

        let mut compilation = Compilation{
            main_path: Some(absolute_path.clone()),
            modules: HashMap::new(),
            root_name: Some(path_to_module_reference(&boxed))
        };
        let just_file = PathBuf::from(absolute_path.file_name().unwrap()).into_boxed_path();
        compilation.compile_tree(&Box::from(absolute_path.parent().unwrap()), &just_file);
        return compilation;
    }

    /// Get the full record type of a submodule.
    /// a.b.c turns into Record(a => Record(b => Record(c => functions_in_c)))
    fn get_submodule_type(sub_module: &CompiledModule, import: &Import) -> Type {

        // Add the types of all the imported functions to the record type.
        let mut record_type = BTreeMap::new();
        for func_dec in &sub_module.ast.data.declarations {
            let func_type = sub_module.context.type_map.get(&func_dec.id).unwrap().clone();
            record_type.insert(func_dec.data.get_name(), func_type);
        }

        // The type of the full module (including all the parent modules).
        let module_type = Type::flatten_to_module(&import.path, record_type);

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
    pub fn compile_tree(&mut self, base_dir: &Box<Path>, file_name: &Box<Path>) {
        let mut f = File::open(base_dir.join(file_name)).expect("File not found");
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
                    self.compile_tree(base_dir, &path);
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
        let mut init_context = Context2::empty();
        let scope_id = init_context.new_scope(init_scope);

        // 
        let context = parsed_module.scopes_and_types(scope_id, init_context).0;

        // Put the results in the tree.
        let compiled = CompiledModule {
            ast: parsed_module,
            context: context,
            path: file_name.clone(),
            dependencies: dependencies,
            hash: 0
        };
        self.modules.insert(module_name, compiled);
    }
    
    // Generate bytecode for all compiled modules, and output the result to files. Return the output of the main file, if it exists.
    pub fn generate_wast_files(&self, output_dir: &Box<Path>) -> Option<String> {
        // let mut ret_str = None;
        // for (k, v) in self.modules.iter() {
        //     let path_str = k.replace(".", "/");
        //     let relative_path = &Path::new(&path_str);
        //     let bytecode = v.ast.generate_bytecode(&v.context);
        //     let mut output_path = output_dir.join(relative_path);
        //     output_path.set_extension("wat");
        //     match create_dir_all(output_path.parent().unwrap()) {
        //         Ok(_) => {},
        //         Err(x) => panic!("{:?}", x)
        //     };
        //     let outfile = File::create(output_path);
        //     outfile.unwrap().write_all(bytecode.as_bytes()).unwrap();
        //     if Some(k) == self.root_name.as_ref() {
        //         ret_str = Some(bytecode);
        //     }
        // }
        panic!()
        // return ret_str;
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

pub fn compile_from_file(file_name: String) -> (Node<Module>, Context2, String){
    let mut f = File::open(file_name).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents).unwrap();
    panic!()
    // return to_bytecode::<Node<Module>>(file_contents.as_bytes());
}

pub fn to_context<'a, T>(input: &'a [u8]) -> (T, Context2)
where T: Parseable, T: GetContext {
    let new_input = PosStr::from(input);
    let mut result = T::parse(new_input);
    let (id, init) = builtin_context();
    let context = result.scopes_and_types(id, init).0;
    return (result, context);
}

pub fn to_type_rewrites<'a, T>(input: &'a [u8]) -> (T, Context2) 
where T: Parseable, T: GetContext, T: Typed<T>, T: Debug {
    let (result, mut context): (T, Context2) = to_context(input);
    let rewritten = result.type_based_rewrite(&mut context);
    return (rewritten, context);
}



// pub fn to_bytecode<'a, T>(input: &'a [u8]) -> (T, Context2, String) 
// where T: Parseable, T: GetContext, T: Typed<T>, T: ToBytecode, T: Debug {
//     let (result, context): (T, Context2) = to_type_rewrites(input);
//     let bytecode = result.generate_bytecode(&context);
//     return (result, context, bytecode);
// }

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{
        read_to_string, read_dir
    };

    fn compile_folder(subfolder: &str) {
        let folder_path = format!("./test_data/{}", subfolder);
        let output_path = format!("./test_data/{}/outputs", subfolder);
        let file_path = format!("{}/file_1.gr", folder_path);
        let compiled = Compilation::compile(file_path);
        let _ = compiled.generate_wast_files(&Box::from(Path::new(&output_path)));
        let paths = read_dir(folder_path).unwrap();
        for path in paths {
            let p = path.unwrap().path();
            let is_gr = match p.extension() {
                Some(s) => s == "gr",
                None => false
            };
            if is_gr {
                let name = p.file_stem();
                let output_file = format!("{}/{}.wat", output_path, name.unwrap().to_str().unwrap());
                let expected_file = format!("{}/{}_expected.wat", output_path, name.unwrap().to_str().unwrap());
                let actual = read_to_string(output_file).unwrap();
                let expected = read_to_string(expected_file).unwrap();
                assert_eq!(actual, expected);
            }
        }
    }

    #[test]
    fn simple_imports_compile_test() {
        compile_folder("simple_imports_test");
    }

    #[test]
    fn import_calls_test() {
        compile_folder("import_calls_test");
    }
}
