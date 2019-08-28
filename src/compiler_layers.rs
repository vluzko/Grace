use std::collections::HashMap;
use std::fmt::Debug;

use p_pos::Parseable;
use position_tracker::PosStr;
use scoping;
use scoping::Scoped;
use typing;
use typing::Typed;
use bytecode::ToBytecode;

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


pub fn to_scopes<'a, T>(input: &'a [u8]) -> (T, scoping::Context)
where T: Parseable, T: Scoped<T> {
    let new_input = PosStr::from(input);
    let mut result = T::parse(new_input);
    let (id, mut init) = scoping::initial_context();
    let context = result.gen_scopes(id, &init);
    init.extend(context);
    // println!("\ninit context: {:?}.\nNew context: {:?}", init, context);
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

pub fn to_bytecode<'a, T>(input: &'a [u8]) -> String 
where T: Parseable, T: Scoped<T>, T: Typed<T>, T: ToBytecode, T: Debug {
    let (result, context, mut type_map): (T, scoping::Context, HashMap<usize, typing::Type>) = to_type_rewrites(input);
    return result.generate_bytecode(&context, &mut type_map);
}


