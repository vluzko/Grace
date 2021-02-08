/// Low-level representation of WebAssembly.
use itertools::join;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::convert::From;
use std::fmt;

use petgraph::{Outgoing, visit::EdgeRef};


use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Module, Expr, Stmt, BinaryOperator, ComparisonOperator, UnaryOperator, Identifier};
use type_checking::types::Type;
use type_checking::scoping::{Context, GetContext};

/// A representation of a WASM module.
/// Includes function declarations, imports, and memory declarations.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMModule {
    pub imports: Vec<WASMImport>,
    pub functions: Vec<WASMFunc>,
    // (Trait name, Struct name, Function declaration)
    pub trait_implementations: Vec<WASMFunc>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMImport {
    pub path: String,
    pub value: String,
    pub internal_name: String,
    pub params: Vec<(String, WASMType)>,
    pub return_type: WASMType,

}

/// A WASM function declaration
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMFunc {
    pub name: String,
    pub args: Vec<(String, WASMType)>,
    pub locals: Vec<(String, WASMType)>,
    pub result: Option<WASMType>,
    pub code: Vec<WASM>
}

/// A WASM expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    End,
    If,
    Else,
    Branch(usize),
    BranchIf(usize),
    Operation(WASMOperator, WASMType),
    Const(String, WASMType),
    Call(String),
    Get(String),
    Set(String),
    Tee(String),
    Load(WASMType),
    Store(WASMType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMOperator {
    Add,
    Sub,
    Mult,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMType {
    i64,
    i32,
    f64,
    f32
}

impl WASMType {
    pub fn size(&self) -> usize {
        return match self {
            WASMType::i32 | WASMType::f32 => 1,
            WASMType::i64 | WASMType::f64 => 2
        };
    }
}

/// Convert a module to LLR.
pub fn module_to_llr(module: &Node<Module>, context: &Context, cfg_map: &HashMap<Identifier, Cfg>) -> WASMModule {
    let mut imports = vec!();
    let mut functions = vec!();
    let mut trait_implementations = vec!();

    for import in &module.data.imports {
        let typing_info = context.get_node_type(import.id);
        for value in &import.values {
            //get params and return type
            let (wasm_args, wasm_return) = match typing_info.resolve_attribute(value) {
                // convert everything to WASMTypes
                Type::Function(ref args, ref return_type) => {
                    let wasm_args = args.iter().map( |(n, t) | (n.name.clone(), WASMType::from(t))).collect();
                    let wasm_return = WASMType::from(&**return_type); 
                    (wasm_args, wasm_return)
                },
                Type::Named(name) => {
                    let full_name = format!("{}.{}", import.string_ref(), name);
                    let actual_type = context.get_defined_type(&Identifier::from(full_name));
                    let (args, return_type) = actual_type.get_constructor_type();
                    let wasm_args = args.iter().map( |(n, t) | (n.name.clone(), WASMType::from(t))).collect();
                    let wasm_return = WASMType::from(&return_type); 
                    (wasm_args, wasm_return)
                },
                x => panic!("Wrong import type: {:?}", x)
            };
            let joined_path = join(import.path.iter().map(|x| x.name.clone()), ".");
            let internal_name = format!(".{}.{}", joined_path, value);
            let wasm_import = WASMImport {
                path: joined_path,
                value: value.name.clone(),
                internal_name: internal_name,
                params: wasm_args,
                return_type: wasm_return
            };
            imports.push(wasm_import);
        }
    }

    for declaration in &module.data.functions {
        functions.push(handle_declaration(declaration, context, cfg_map));
    }

    for declaration in &module.data.structs {
        functions.push(handle_declaration(declaration, context, cfg_map));
    }

    for (trait_name, internal_type_name, function_decs) in &module.data.trait_implementations {
        for declaration in function_decs {
            let name_prefix = format!("{}.{}", trait_name, internal_type_name);
            let ambiguous_name_func = handle_trait_func_dec(declaration, &name_prefix, context, cfg_map);
            let full_name = format!("{}.{}.{}", trait_name, internal_type_name, ambiguous_name_func.name);
            let full_name_func = WASMFunc{
                name: full_name,
                args: ambiguous_name_func.args,
                locals: ambiguous_name_func.locals,
                result: ambiguous_name_func.result,
                code: ambiguous_name_func.code
            };
            trait_implementations.push(full_name_func);
        }
    }
    return WASMModule {
        imports: imports,
        functions: functions,
        trait_implementations: trait_implementations
    };
}

// Helper for module_to_llr
pub fn handle_declaration(declaration: &Node<Stmt>, context: &Context, cfg_map: &HashMap<Identifier, Cfg>) -> WASMFunc {
    return match declaration.data {
        Stmt::FunctionDecStmt{ref name, ref args, ref kwargs, ref return_type, ..} => {
            let local_variables = declaration.get_true_declarations(context);
            let locals_with_wasm_types: Vec<(String, WASMType)> = local_variables.iter().map(
                |(n, t)| (n.name.clone(), WASMType::from(t))).collect();
            let cfg = cfg_map.get(name).unwrap();
            let block_llr = cfg.to_llr(context);
            let mut wasm_args: Vec<(String, WASMType)>  = args.iter().map(|(name, t)| (name.name.clone(), WASMType::from(t))).collect();
            // Add kwargs
            wasm_args.append(&mut kwargs.iter().map(|(name, t, _)| (name.name.clone(), WASMType::from(t))).collect());
            let wasm_func = WASMFunc {
                name: name.name.clone(),
                args: wasm_args,
                locals: locals_with_wasm_types,
                result: Option::<WASMType>::from(return_type),
                code: block_llr
            };
            wasm_func
        },
        Stmt::StructDec{ref name, ref fields} => {
            let wasm_args: Vec<(String, WASMType)>  = fields.iter().map(|(name, t)| (name.name.clone(), WASMType::from(t))).collect();
            let mut block_llr = vec!();
            let num_words: usize = wasm_args.iter().map(|(_, t)| t.size()).sum();
            block_llr.push(WASM::Const(format!("{}", num_words), WASMType::i32));
            block_llr.push(WASM::Call(".memory_management.alloc_words".to_string()));
            block_llr.push(WASM::Set(".x".to_string()));
            for (index, (field_name, _)) in fields.iter().enumerate() {
                block_llr.push(WASM::Get(".x".to_string()));
                block_llr.push(WASM::Const(format!("{}", 8+index*4), WASMType::i32));
                block_llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
                block_llr.push(WASM::Get(field_name.name.clone()));
                block_llr.push(WASM::Call(".memory_management.set".to_string()));
            }
            block_llr.push(WASM::Get(".x".to_string()));
            block_llr.push(WASM::Const("8".to_string(), WASMType::i32));
            block_llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
            let wasm_func = WASMFunc {
                name: name.name.clone(),
                args: wasm_args,
                locals: vec!((".x".to_string(), WASMType::i32)),
                result: Some(WASMType::i32),
                code: block_llr
            };
            wasm_func                
        }
        _ => panic!()
    }
}

pub fn handle_trait_func_dec(declaration: &Node<Stmt>, name_prefix: &String, context: &Context, cfg_map: &HashMap<Identifier, Cfg>) -> WASMFunc {
    return match &declaration.data {
        &Stmt::FunctionDecStmt{ref name, ref args, ref kwargs, ref return_type, ..} => {
            // Get local variables as WASM.
            let local_variables = declaration.get_true_declarations(context);
            let locals_with_wasm_types: Vec<(String, WASMType)> = local_variables.iter().map(
                |(n, t)| (n.name.clone(), WASMType::from(t))).collect();

            // Get function block LLR.
            let full_name = Identifier::from(format!("{}.{}", name_prefix, name.name));
            let cfg = cfg_map.get(&full_name).unwrap();
            let block_llr = cfg.to_llr(context);

            // Add args
            let mut wasm_args: Vec<(String, WASMType)>  = args.iter().map(|(name, t)| (name.name.clone(), WASMType::from(t))).collect();

            // Add kwargs
            wasm_args.append(&mut kwargs.iter().map(|(name, t, _)| (name.name.clone(), WASMType::from(t))).collect());
            let wasm_func = WASMFunc {
                name: name.name.clone(),
                args: wasm_args,
                locals: locals_with_wasm_types,
                result: Option::<WASMType>::from(return_type),
                code: block_llr
            };
            wasm_func
        }
        x => panic!("Found {:?} inside a trait implementation block. Only function declarations are allowed here.", x)
    }
}

pub trait ToLLR {
    fn to_llr(&self, context: &Context) -> Vec<WASM>;
}

impl ToLLR for Cfg {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        let mut wasm = vec!();

        let mut unvisited = vec!(self.entry_index.unwrap());

        while let Some(current_index) = unvisited.pop() {
            let node = self.graph.node_weight(current_index).unwrap();
            wasm.append(&mut node.to_llr(context));
            match node {
                CfgVertex::Block(_) | CfgVertex::Else | CfgVertex::End | CfgVertex::Entry => {
                    let mut n_count = 0;
                    let edges = self.graph.edges_directed(current_index, Outgoing);
                    for edge in edges {
                        unvisited.push(edge.target());
                        n_count += 1;
                        assert_eq!(n_count, 1);
                    }
                },
                CfgVertex::IfStart(_) | CfgVertex::LoopStart(_) => {
                    let mut n_count = 0;
                    let edges = self.graph.edges_directed(current_index, Outgoing);
                    let mut false_block = None;
                    let mut true_block = None;
                    for edge in edges {
                        if edge.weight() == &true {
                            true_block = Some(edge.target());
                        } else {
                            false_block = Some(edge.target());
                        }
                        n_count += 1;
                    }
                    assert_eq!(n_count, 2);

                    match false_block {
                        Some(x) => unvisited.push(x),
                        None => panic!()
                    };

                    match true_block {
                        Some(x) => unvisited.push(x),
                        None => panic!()
                    }
                },
                CfgVertex::Break(_) | CfgVertex::Continue(_) | CfgVertex::Exit => {}
            };
        }
        return wasm;
    }
}

impl ToLLR for CfgVertex {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match &self {
            CfgVertex::Block(statements) => {
                let mut wasm = vec!();
                for stmt in statements {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm
            },
            CfgVertex::IfStart(expression) => {
                let mut wasm = expression.to_llr(context);
                wasm.push(WASM::If);
                wasm
            },
            CfgVertex::LoopStart(expression) => {
                let mut wasm = expression.to_llr(context);
                wasm.push(WASM::Loop);
                wasm
            },
            CfgVertex::Break(block) => {
                let mut wasm = vec!();
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm.push(WASM::Branch(0));
                wasm
            },
            CfgVertex::Continue(block) => {
                let mut wasm = vec!();
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm.push(WASM::Branch(1));
                wasm
            },
            CfgVertex::Else => vec!(WASM::Else),
            CfgVertex::End => vec!(WASM::End),
            CfgVertex::Entry | CfgVertex::Exit => vec!(),
        } ;
    }
}

impl ToLLR for Node<CfgStmt> {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match self.data {
            CfgStmt::Assignment {ref name, ref expression} | CfgStmt::Let {ref name, ref expression} => {
                let mut expr_wasm = expression.to_llr(context);
                expr_wasm.push(WASM::Set(name.name.clone()));
                expr_wasm
            },
            CfgStmt::Return (ref val) | CfgStmt::Yield (ref val) | CfgStmt::Branch (ref val) => {
                val.to_llr(context)
            }
        }
    }
}

impl ToLLR for Node<Expr> {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match &self.data {
            Expr::BinaryExpr{ref left, ref right, ref operator} => {
                let mut llr = left.to_llr(context);
                llr.append(&mut right.to_llr(context));
                let left_id_type = context.get_node_type(left.id);
                let left_wasm_type = WASMType::from(&left_id_type);
                let right_id_type = context.get_node_type(right.id);
                let right_wasm_type = WASMType::from(&right_id_type);
                assert_eq!(left_wasm_type, right_wasm_type);
                llr.push(WASM::Operation(WASMOperator::from(operator), left_wasm_type));
                llr
            },
            Expr::ComparisonExpr{ref left, ref right, ref operator} => {
                let mut llr = left.to_llr(context);
                llr.append(&mut right.to_llr(context));
                let left_id_type = context.get_node_type(left.id);
                let left_wasm_type = WASMType::from(&left_id_type);
                let right_id_type = context.get_node_type(right.id);
                let right_wasm_type = WASMType::from(&right_id_type);
                assert_eq!(left_wasm_type, right_wasm_type);
                // Convert operator to a wasm operator
                llr.push(WASM::Operation(WASMOperator::from(operator), left_wasm_type));
                llr
            },
            Expr::FunctionCall{ref function, ref args, ..} => {
                let mut llr = vec!();
                for arg in args {
                    llr.append(&mut arg.to_llr(context));
                }
                match &function.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    },
                    Expr::ModuleAccess(_, ref path) => {
                        let func_name = join(path.iter().map(|x| x.name.clone()), ".");
                        llr.push(WASM::Call(format!(".{}", func_name.clone())));
                    },
                    Expr::TraitAccess{ref base, ref trait_name, ref attribute} => {
                        let base_type = context.get_node_type(base.id);
                        let full_func_name = format!("{}.{}.{}", trait_name, base_type.trait_impl_name(), attribute);
                        llr.push(WASM::Call(format!(".{}", full_func_name)));
                    },
                    x => panic!("FunctionCall to_llr not implemented for :{:?}", x)
                }
                llr
            },
            Expr::StructLiteral{ref base, ref fields} => {
                let mut llr = vec!();
                for field in fields {
                    llr.append(&mut field.to_llr(context));
                }
                match &base.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    },
                    Expr::ModuleAccess(_, ref path) => {
                        let func_name = join(path.iter().map(|x| x.name.clone()), ".");
                        llr.push(WASM::Call(format!(".{}", func_name.clone())));
                    },
                    x => panic!("StructLiteral to_llr not implemented for :{:?}", x)
                }
                llr
            },
            Expr::AttributeAccess{ref base, ref attribute} => {
                let mut llr = vec!();
                // Get the address where the result of the base expression is stored.
                llr.append(&mut base.to_llr(context));
                let base_type = context.get_node_type(base.id);
                match base_type {
                    Type::Record(ref names, ref fields) => {
                        let attr_type = fields.get(attribute).unwrap();
                        // Calculate the offset of `attribute` from the start of the expression result.
                        let offset = calculate_offset(attribute, names, fields);
                        llr.push(WASM::Const(format!("{}", offset), WASMType::i32));
                        // Add the offset and the address
                        llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
                        // Get the value at that address.
                        llr.push(WASM::Load(WASMType::from(attr_type)));
                    },
                    x => panic!("Cannot access attribute of: {:?}", x)
                }

                llr
            },
            Expr::UnaryExpr{ref operator, ref operand} => {
                let llr = operand.to_llr(context);
                match operator {
                    UnaryOperator::Convert(to_type, from_type) => match to_type {
                        Type::Gradual(_) => match from_type {
                            Type::Gradual(_) => {},
                            _ => panic!()
                        },
                        _ => panic!()

                    },
                    x => panic!("Got an unexpected unary operator: {:?}", x)
                };
                llr
            },
            Expr::Int(ref value) | Expr::Float(ref value) => {
                let id_type = context.get_node_type(self.id);
                let wasm_type = WASMType::from(&id_type);
                vec!(WASM::Const(value.clone(), wasm_type))
            },
            Expr::Bool(ref value) => {
                return match value {
                    true => vec!(WASM::Const("1".to_string(), WASMType::i32)),
                    false => vec!(WASM::Const("0".to_string(), WASMType::i32))
                }
            },
            Expr::IdentifierExpr(ref identifier) => vec!(WASM::Get(identifier.name.clone())),
            Expr::VecLiteral(ref exprs) => {
                let mut llr = vec!();
                let t = context.get_node_type(self.id);
                let vector_size = exprs.len() * t.size() + 3;
                llr.push(WASM::Const(format!("{}", vector_size), WASMType::i32));
                llr.push(WASM::Call(".memory_management.alloc_words".to_string()));

                for expr in exprs {
                    llr.append(&mut expr.to_llr(context));
                    llr.push(WASM::Call(".memory_management.tee_memory".to_string()));
                    llr.push(WASM::Const(format!("{}", t.size()*4), WASMType::i32));
                    llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
                }
                llr.push(WASM::Const(format!("{}", t.size()*4*(exprs.len())), WASMType::i32));
                llr.push(WASM::Operation(WASMOperator::Sub, WASMType::i32));
                llr
                //TODO this block needs a test case 
            },
            Expr::TupleLiteral(ref exprs) => {
                let mut llr = vec!();
                let t = context.get_node_type(self.id);
                let individual_types = match &t {
                    Type::Product(ref types) => types.clone(),
                    _ => panic!()
                };
                let vector_size = t.size() + 3;
                llr.push(WASM::Const(format!("{}", vector_size), WASMType::i32));
                llr.push(WASM::Call(".memory_management.alloc_words".to_string()));

                for (i, expr) in exprs.iter().enumerate() {
                    llr.append(&mut expr.to_llr(context));
                    llr.push(WASM::Call(".memory_management.tee_memory".to_string()));
                    llr.push(WASM::Const(format!("{}", individual_types[i].size()*4), WASMType::i32));
                    llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
                }
                llr.push(WASM::Const(format!("{}", t.size()*4), WASMType::i32));
                llr.push(WASM::Operation(WASMOperator::Sub, WASMType::i32));
                llr
                //TODO this block needs a test case 
            },
            x => panic!("to_llr not implemented for: {:?}", x)
        }
    }
}

/// Calculate the offset of a field in a tuple.
fn calculate_offset(name: &Identifier, order: &Vec<Identifier>, type_map: &BTreeMap<Identifier, Type>) -> usize {
    let mut offset = 8;
    for val in order {
        if val == name {
            break
        } else {
            offset += type_map.get(&val).unwrap().size();
        }
    }
    return offset;
}


/// Implementations of Rust builtin traits.
pub mod rust_trait_impls {
    use super::*;

    impl From<&Type> for Option<WASMType> {
        fn from(input: &Type) -> Self {
            match &input {
                Type::i32 => Some(WASMType::i32),
                Type::i64 => Some(WASMType::i64),
                Type::f32 => Some(WASMType::f32),
                Type::f64 => Some(WASMType::f64),
                Type::boolean => Some(WASMType::i32),
                Type::Sum(..) => Some(WASMType::i32),
                Type::Named(..) => Some(WASMType::i32),
                Type::Refinement(ref base, ..) => Option::<WASMType>::from(&**base),
                Type::Gradual(_) => Some(WASMType::i32),
                Type::empty => None,
                x => panic!("Tried to convert {:?} to WASM", x)
            }
        }
    }

    impl From<&Type> for WASMType {
        fn from(input: &Type) -> Self {
            match &input {
                Type::i32 => WASMType::i32,
                Type::i64 => WASMType::i64,
                Type::f32 => WASMType::f32,
                Type::f64 => WASMType::f64,
                Type::boolean => WASMType::i32,
                Type::Sum(..) => WASMType::i32,
                Type::Named(..) => WASMType::i32,
                Type::Refinement(ref base, ..) => WASMType::from(&**base),
                Type::Gradual(_) => WASMType::i32,
                x => panic!("Tried to convert {:?} to WASM", x)
            }
        }
    }

    impl From<&BinaryOperator> for WASMOperator {
        fn from(input: &BinaryOperator) -> Self {
            return match input {
                BinaryOperator::Add => WASMOperator::Add,
                BinaryOperator::Sub => WASMOperator::Sub,
                BinaryOperator::Mult => WASMOperator::Mult,
                BinaryOperator::Div => WASMOperator::Div,
                _ => panic!()
            };
        }
    }

    impl From<&ComparisonOperator> for WASMOperator {
        fn from(input: &ComparisonOperator) -> Self {
            return match input {
                ComparisonOperator::Equal => WASMOperator::Eq,
                ComparisonOperator::Less => WASMOperator::Lt,
                ComparisonOperator::Greater => WASMOperator::Gt,
                ComparisonOperator::LessEqual => WASMOperator::Le,
                ComparisonOperator::GreaterEqual => WASMOperator::Ge,
                _ => panic!()
            };
        }
    }

    impl fmt::Display for WASMType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", match self {
                WASMType::i32 => "i32",
                WASMType::i64 => "i64",
                WASMType::f32 => "f32",
                WASMType::f64 => "f64"
            })
        }
    }

    impl fmt::Display for WASMOperator {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", match self {
                WASMOperator::Add => "add",
                WASMOperator::Sub => "sub",
                WASMOperator::Mult => "mul",
                WASMOperator::Div => "div",
                WASMOperator::Eq => "eq",
                WASMOperator::Ne => "ne",
                x => panic!("Display not implemented for WASMOperator: {:?}", x)
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read;
    use std::fs::File;

    use compiler_layers;

    #[test]
    fn trait_impl_test() {
        let mut f = File::open("test_data/trait_impl_test.gr").expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        let (_, _, _, llr) = compiler_layers::to_llr(file_contents.as_bytes());
        let func_names: Vec<String> = llr.functions.iter().map(|x| x.name.clone()).collect();
        assert_eq!(func_names, vec!("call_trait_func".to_string(), "teststruct".to_string()));

        let trait_impl_names: Vec<String> = llr.trait_implementations.iter().map(|x| x.name.clone()).collect();
        assert_eq!(trait_impl_names, vec!("testtrait.teststruct.baz".to_string()));
        println!("{:?}", llr);
    }


    #[cfg(test)]
    mod exprs {

        #[test]
        fn test_constants() {
            panic!()
        }
    }

}