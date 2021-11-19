//! Compilation.
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::fs::{canonicalize, create_dir_all, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use itertools::join;

use expression::{Identifier, Import, Module, Node};
use parser::base::Parseable;
use parser::position_tracker::PosStr;
use type_checking::context::Context;
use type_checking::type_check::GetContext;
use type_checking::types::Type;

use grace_error::GraceError;
use bytecode::ToBytecode;
use cfg::{module_to_cfg, Cfg, CfgMap};
use general_utils::{extend_map, get_next_id, join as join_vec};
use llr::{module_to_llr, WASMModule};
use pre_cfg_rewrites::TypeRewritable;

#[derive(Debug, Clone)]
pub struct CompiledModule {
    pub ast: Node<Module>,
    pub context: Context,
    pub cfg_map: HashMap<Identifier, Cfg>,
    pub llr: WASMModule,
    // The path to the module file.
    pub path: Box<Path>,
    // The dependencies of this module.
    pub dependencies: Vec<String>,
    // The MD5 hash of the *file* describing the module.
    pub hash: u64,
}

impl PartialEq for CompiledModule {
    fn eq(&self, other: &Self) -> bool {
        return self.hash == other.hash;
    }
}

impl Eq for CompiledModule {}

impl CompiledModule {
    pub fn get_type(&self) -> Type {
        let mut attribute_map = BTreeMap::new();
        let mut attribute_order = vec![];
        for func_dec in &self.ast.data.functions {
            let func_type = self.context.type_map.get(&func_dec.id).unwrap().clone();
            attribute_order.push(func_dec.data.get_name());
            attribute_map.insert(func_dec.data.get_name(), func_type);
        }
        for struct_dec in &self.ast.data.structs {
            let struct_type = self.context.type_map.get(&struct_dec.id).unwrap().clone();
            attribute_order.push(struct_dec.data.get_name());
            attribute_map.insert(struct_dec.data.get_name(), struct_type);
        }

        return Type::Record(attribute_order, attribute_map);
    }

    pub fn get_internal_module_name(idents: &Vec<Identifier>) -> String {
        return join(idents.iter().map(|x| x.name.clone()), ".");
    }
}

#[derive(Debug, Clone)]
pub struct Compilation {
    /// The path to the main file or folder.
    pub main_path: Option<Box<Path>>,
    /// All parsed modules.
    pub modules: HashMap<String, CompiledModule>,
    pub root_name: Option<String>,
}

impl Compilation {
    pub fn compile(file_name: &String) -> Compilation {
        let path = Path::new(file_name);
        let absolute_path = canonicalize(path).unwrap().into_boxed_path();

        // Panics if the file_name ends in ".."
        let boxed = Box::from(Path::new(path.file_name().unwrap()));

        let compilation = Compilation {
            main_path: Some(absolute_path.clone()),
            modules: HashMap::new(),
            root_name: Some(path_to_module_reference(&boxed)),
        };
        let just_file = PathBuf::from(absolute_path.file_name().unwrap()).into_boxed_path();
        let result = compilation.compile_tree(&Box::from(absolute_path.parent().unwrap()), &just_file);
        return match result {
            Ok(c) => c,
            Err(e) => {
                panic!("Compilation failed with error: {:?}", e)
            }
        }
    }

    /// Get the full record type of a submodule.
    /// a.b.c turns into Record(a => Record(b => Record(c => functions_in_c)))
    fn get_submodule_type(submodule: &CompiledModule, import: &Import) -> Type {
        // Add the types of all the imported functions to the record type.
        let mut record_type = BTreeMap::new();
        for func_dec in &submodule.ast.data.functions {
            let func_type = submodule
                .context
                .type_map
                .get(&func_dec.id)
                .unwrap()
                .clone();
            record_type.insert(func_dec.data.get_name(), func_type);
        }
        for struct_dec in &submodule.ast.data.structs {
            let struct_type = submodule
                .context
                .type_map
                .get(&struct_dec.id)
                .unwrap()
                .clone();
            record_type.insert(struct_dec.data.get_name(), struct_type);
        }

        // The type of the full module (including all the parent modules).
        let module_type = Type::flatten_to_module(&import.path, record_type);

        return module_type;
    }

    /// Compile the module tree rooted at the given file name.
    pub fn compile_tree(mut self, base_dir: &Box<Path>, file_name: &Box<Path>) -> Result<Compilation, GraceError> {
        let mut f = File::open(base_dir.join(file_name)).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        // Parse the module
        let mut parsed_module =
            <Node<Module> as Parseable>::parse(PosStr::from(file_contents.as_bytes()));
        let module_name = path_to_module_reference(&file_name);

        // Set everything up for compiling the dependencies.
        let mut new_imports = vec![];
        let mut dependencies = vec![];
        let mut init_context = Context::builtin();

        // Add builtin imports
        for (import, t) in default_imports().into_iter() {
            init_context.append_import(&import);
            init_context.add_type(import.id, t);
            new_imports.push(Box::new(import));
        }

        // Compile dependencies
        for import in &parsed_module.data.imports {
            let submodule_name = join(import.path.iter().map(|x| x.name.clone()), ".");
            // TODO: Cleanup: Just overwrite parsed_module.data.imports directly instead of waiting.
            let (res, new_import) = self.add_import(base_dir, import, &mut init_context);
            self = res;

            new_imports.push(Box::new(new_import));
            dependencies.push(submodule_name);
        }

        parsed_module.data.imports = new_imports;

        let (mut context, _) = parsed_module.scopes_and_types(init_context.root_id, init_context)?;
        // match context_res {
            // Ok((mut context, _)) => {
                let rewritten = parsed_module.type_based_rewrite(&mut context);
                let cfg_map = module_to_cfg(&rewritten, &context);
                let wasm = module_to_llr(&rewritten, &context, &cfg_map);

                // Put the results in the tree.
                let compiled = CompiledModule {
                    ast: rewritten,
                    context: context,
                    cfg_map: cfg_map,
                    llr: wasm,
                    path: file_name.clone(),
                    dependencies: dependencies,
                    hash: 0,
                };
                self.modules.insert(module_name, compiled);
            // }
            // Err(e) => panic!("Unimplemented error handling: {:?}", e),
        // };

        return Ok(self);
    }

    /// Compile an import, add the module to the compilation object, and return the updated Import object.
    fn add_import(
        mut self,
        base_dir: &Box<Path>,
        import: &Import,
        context: &mut Context,
    ) -> (Compilation, Import) {
        let path = module_path_to_path(&import.path);
        let submodule_name = join(import.path.iter().map(|x| x.name.clone()), ".");

        let submodule = match self.modules.get(&submodule_name) {
            Some(x) => x,
            None => {
                self = self.compile_tree(base_dir, &path).unwrap();
                self.modules.get(&submodule_name).unwrap()
            }
        };

        // A vector containing all the names of the imported functions.
        let func_exports = submodule
            .ast
            .data
            .functions
            .iter()
            .map(|x| x.data.get_name().clone())
            .collect();
        let struct_exports = submodule
            .ast
            .data
            .structs
            .iter()
            .map(|x| x.data.get_name().clone())
            .collect();
        let exports = join_vec(func_exports, struct_exports);

        // Add the imported functions to scope.
        context.append_import(&import);

        // The type of the full module (including all the parent modules).
        let module_type = Compilation::get_submodule_type(submodule, &import);
        context.add_type(import.id, module_type);

        // Import struct types from the module
        for (def_name, def_type) in &submodule.context.defined_types {
            let new_type_name = Identifier::from(format!("{}.{}", import.string_ref(), def_name));
            context.define_type(new_type_name, def_type.clone());
        }

        let new_import = Import {
            id: import.id,
            path: import.path.clone(),
            alias: import.alias.clone(),
            values: exports,
        };
        return (self, new_import);
    }

    // Generate bytecode for all compiled modules, and output the result to files. Return the output of the main file, if it exists.
    pub fn generate_wast_files(&self, output_dir: &Box<Path>) -> Option<String> {
        let mut ret_str = None;
        for (k, v) in self.modules.iter() {
            let path_str = k.replace(".", "/");
            let relative_path = &Path::new(&path_str);
            let bytecode = v.llr.to_bytecode(&v.context);
            let mut output_path = output_dir.join(relative_path);
            output_path.set_extension("wat");
            match create_dir_all(output_path.parent().unwrap()) {
                Ok(_) => {}
                Err(x) => panic!("{:?}", x),
            };
            let outfile = File::create(output_path);
            outfile.unwrap().write_all(bytecode.as_bytes()).unwrap();
            if Some(k) == self.root_name.as_ref() {
                ret_str = Some(bytecode);
            }
        }
        return ret_str;
    }

    pub fn compile_from_string(input: &String) -> Compilation {
        let mut compilation = Compilation::empty();
        // Parse the module
        let mut parsed_module = <Node<Module> as Parseable>::parse(PosStr::from(input));

        // Set everything up for compiling the dependencies.
        let mut new_imports = vec![];

        let mut init_context = Context::empty();

        // Add builtin imports
        for (import, t) in default_imports().into_iter() {
            init_context.append_import(&import);
            init_context.add_type(import.id, t);
            new_imports.push(Box::new(import));
        }

        // No imports if you're compiling from a string.
        if parsed_module.data.imports.len() > 0 {
            panic!()
        }

        let context_res = parsed_module.scopes_and_types(init_context.root_id, init_context);
        return match context_res {
            Ok((context, _)) => {
                let cfg_map = module_to_cfg(&parsed_module, &context);
                let wasm = module_to_llr(&parsed_module, &context, &cfg_map);

                // Put the results in the tree.
                let compiled = CompiledModule {
                    ast: parsed_module,
                    context: context,
                    cfg_map: cfg_map,
                    llr: wasm,
                    path: Box::from(Path::new(".")),
                    dependencies: vec![],
                    hash: 0,
                };
                compilation.modules.insert("$cli".to_string(), compiled);
                compilation
            }
            Err(_) => panic!("Unimplemented error handling"),
        };
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

/// Return the default imports that all files need.
fn default_imports() -> Vec<(Import, Type)> {
    let mm_id = get_next_id();

    let mem_management = Import {
        id: mm_id,
        path: vec![Identifier::from("memory_management")],
        alias: Some(Identifier::from(".memory_management")),
        values: vec![
            Identifier::from("alloc_words"),
            Identifier::from("free_chunk"),
            Identifier::from("copy_many"),
            Identifier::from("tee_memory"),
        ],
    };
    let alloc_and_free_type = Type::Function(
        vec![(Identifier::from("a"), Type::i32)],
        Box::new(Type::i32),
    );
    let copy_type = Type::Function(
        vec![
            (Identifier::from("a"), Type::i32),
            (Identifier::from("b"), Type::i32),
            (Identifier::from("size"), Type::i32),
        ],
        Box::new(Type::i32),
    );
    let tee_type = Type::Function(
        vec![
            (Identifier::from("loc"), Type::i32),
            (Identifier::from("value"), Type::i32),
        ],
        Box::new(Type::i32),
    );
    let mut mem_management_func_map = BTreeMap::new();
    mem_management_func_map.insert(Identifier::from("alloc_words"), alloc_and_free_type.clone());
    mem_management_func_map.insert(Identifier::from("free_chunk"), alloc_and_free_type);
    mem_management_func_map.insert(Identifier::from("copy_many"), copy_type);
    mem_management_func_map.insert(Identifier::from("tee_memory"), tee_type);

    let mem_type = Type::Module(
        vec![Identifier::from("memory_management")],
        mem_management_func_map,
    );

    let bin_ops_id = get_next_id();

    let binary_operations = Import {
        id: bin_ops_id,
        path: vec![Identifier::from("gradual_binary_ops")],
        alias: Some(Identifier::from(".gradual_binary_ops")),
        values: vec![Identifier::from("call_gradual")],
    };
    let call_gradual_type = Type::Function(
        vec![
            (Identifier::from("i"), Type::i32),
            (Identifier::from("a"), Type::i32),
            (Identifier::from("b"), Type::i32),
        ],
        Box::new(Type::i32),
    );
    let mut bin_ops_func_map = BTreeMap::new();
    bin_ops_func_map.insert(Identifier::from("call_gradual"), call_gradual_type.clone());

    let binary_operations_type = Type::Module(
        vec![Identifier::from("gradual_binary_ops")],
        bin_ops_func_map,
    );

    return vec![
        (mem_management, mem_type),
        (binary_operations, binary_operations_type),
    ];
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
    return join(
        without_extension
            .components()
            .map(|x| x.as_os_str().to_os_string().into_string().unwrap()),
        ".",
    );
}

pub fn compile_from_file(file_name: String) -> (Node<Module>, Context, String) {
    let mut f = File::open(file_name).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents).unwrap();
    panic!()
    // return to_bytecode::<Node<Module>>(file_contents.as_bytes());
}

pub fn to_context<'a, T>(input: &'a [u8]) -> (T, Context)
where
    T: Parseable,
    T: GetContext,
{
    let new_input = PosStr::from(input);
    let mut result = T::parse(new_input);
    let init = Context::builtin();
    let id = init.root_id;
    let context_res = result.scopes_and_types(id, init);
    return match context_res {
        Ok((context, _)) => (result, context),
        x => panic!("COMPILER ERROR: {:?}", x),
    };
}

pub fn to_type_rewrites<'a, T>(input: &'a [u8]) -> (T, Context)
where
    T: Parseable,
    T: GetContext,
    T: TypeRewritable<T>,
    T: Debug,
{
    let (result, mut context): (T, Context) = to_context(input);
    let rewritten = result.type_based_rewrite(&mut context);
    return (rewritten, context);
}

pub fn to_cfg_map<'a>(input: &'a [u8]) -> (Node<Module>, Context, CfgMap) {
    let (module, context) = to_type_rewrites::<Node<Module>>(input);
    let cfg_map = module_to_cfg(&module, &context);
    return (module, context, cfg_map);
}

pub fn to_llr<'a>(input: &'a [u8]) -> (Node<Module>, Context, CfgMap, WASMModule) {
    let (module, context, cfg_map) = to_cfg_map(input);
    let llr = module_to_llr(&module, &context, &cfg_map);
    return (module, context, cfg_map, llr);
}
