use itertools::join;

use scoping::Context;
use llr::{
    WASMModule,
    WASMFunc,
    WASM
};

pub trait ToBytecode {
    /// Generate WAST bytecode from an AST
    /// CONVENTIONS:
    ///     Generated bytecode should not start or end with a newline, *except* for modules / other
    ///     top level functions. Starting and ending newlines are the responsibility of the parent
    ///     node. We don't have to do it this way, but it's important to have a single convention.
    ///     Nodes *do* need to handle newlines at the start and end of their *children*, obviously.
    fn to_bytecode(&self, context: &Context) -> String;
}

impl ToBytecode for WASMModule {
    fn to_bytecode(&self, context: &Context) -> String {
        let mut import_strings = vec!();
        let mut function_declarations = vec!();
        println!("{:?}", self.imports);
        for import in &self.imports {
            //(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
            let params = join(import.params.iter().map(|(n,t)| format!("(param ${} {})", n, t)), " ");
            let res = format!("(result {})", import.return_type);
            let import_string = format!("(import \"{}\" \"{}\" (func ${} {} {}))", 
            import.path, import.value, import.internal_name, params, res);
            import_strings.push(import_string);
        }
        for function in &self.functions {
            let declaration_string = function.to_bytecode(context);
            function_declarations.push(declaration_string)
        }
        return format!("{}\n\n{}", join(import_strings, "\n"), join(function_declarations, "\n\n"));
    }
}

impl ToBytecode for WASMFunc {
    fn to_bytecode(&self, context: &Context) -> String {
        let param_string = join(self.args.iter().map(|(n, t)| format!("(param ${} {})", n, t)), " ");
        let result_string = format!("(result {})", self.result);
        let local_string = join(self.locals.iter().map(|(n, t)| format!("(local ${} {})", n, t)), " ");
        let header = format!("func ${} {} {} {}", self.name, param_string, result_string, local_string);

        let footer = format!("(export \"{}\" (func ${}))", self.name, self.name);
        
        let code_string = join(self.code.iter().map(|x| x.to_bytecode(context)), "\n    ");
        return format!("({}\n{}\n){}", header, code_string, footer);
    }
}

impl ToBytecode for WASM {
    fn to_bytecode(&self, context: &Context) -> String {
        return match self {
            // Control-flow
            WASM::Block => "block $void".to_string(),
            WASM::Loop => "loop $void".to_string(),
            WASM::If => "if".to_string(),
            WASM::Else => "else".to_string(),
            WASM::End => "end".to_string(),
            WASM::Branch(level) => format!("br {}", level),
            WASM::BranchIf(level) => format!("br_if {}", level),
            // Expressions
            WASM::Const(t, val) => format!("{}.const {}", t, val),
            WASM::Call(name) => format!("call ${}", name),
            WASM::Operation(operator, t) => format!("{}.{}", t, operator),
            WASM::Get(name) => format!("get_local ${}", name),
            WASM::Set(name) => format!("set_local ${}", name),
            WASM::Tee(name) => format!("tee_local ${}", name),
            WASM::Load(t) => format!("{}.load", t),
            WASM::Store(t) => format!("{}.store", t),
            x => panic!("WASM to_bytecode not implemented for: {:?}", x)
        };

    }
}


// impl ToBytecode for Node<Module> {

//     /// Generate bytecode for a module.
//     /// Is this code unbelievably ugly? Yes. Can I think of an easy way to make it prettier? No.
//     fn to_bytecode(&self, context: &Context2) -> String {
//         let mut imports = vec!();
//         for import in &self.data.imports {
//             let import_stmt = context.get_declaration(self.scope, &import.path.get(0).unwrap()).unwrap();
//             let import_type = context.type_map.get(&import_stmt.get_id()).unwrap();
//             for value in &import.values {
//                 let mut ident_vec = import.path[1..].to_vec().clone();
//                 ident_vec.push(value.clone());
//                 let func_type = import_type.resolve_nested_record(&ident_vec);
//                 let module_name = CompiledModule::get_internal_module_name(&import.path);
//                 let type_str = func_type.wast_name();
//                 let import_stmt = format!("(import \"{mod}\" \"{func}\" (func $.{mod}.{func} {type}))", 
//                     mod=module_name, 
//                     func=value.name,
//                     type=type_str
//                 );
//                 imports.push(import_stmt);
//             }
//         }

//         let decls = self.data.declarations.iter().map(|x| x.to_bytecode(context));
//         let import_str = itertools::join(imports.iter(), "\n");
//         let joined = itertools::join(decls, "\n");
//         return format!("(module\n\
//         (import \"memory_management\" \"alloc_words\" (func $.memory_management.alloc_words (param $a i32) (result i32)))\n\
//         (import \"memory_management\" \"free_chunk\" (func $.memory_management.free_chunk (param $a i32) (result i32)))\n\
//         (import \"memory_management\" \"copy_many\" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))\n\
//         (import \"memory_management\" \"mem\" (memory (;0;) 1))\n\
//         {}\n\
//         {}\n)\n", import_str, joined).to_string();
//     }
// }



#[cfg(test)]
mod tests {

}
