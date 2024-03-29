//! Low-level representation of WebAssembly.
use itertools::join;
use std::collections::{BTreeMap, HashMap};
use std::convert::From;
use std::fmt;

use petgraph::{visit::EdgeRef, Outgoing};

use cfg::{Cfg, CfgStmt, CfgVertex};
use expression::{BinaryOperator, Expr, Identifier, Module, Node, Stmt, UnaryOperator};
use general_utils;
use grace_error::GraceError;
use type_checking::context::Context;
use type_checking::types::Type;

// These values are hardcoded by memory_management::create_chunk
// It's one word for the next chunk pointer and one word
// for the size of the chunk.
/// The size of the header of a chunk in words.
static HEADER_SIZE_WORDS: usize = 2;
/// The size of the header of a chunk in bytes.
static HEADER_SIZE_BYTES: usize = HEADER_SIZE_WORDS * 4;

/// A representation of a WASM module.
/// Includes function declarations, imports, and memory declarations.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMModule {
    pub imports: Vec<WASMImport>,
    pub functions: Vec<WASMFunc>,
    // (Trait name, Struct name, Function declaration)
    pub trait_implementations: Vec<WASMFunc>,
}

/// A WASM import statement
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
    pub code: Vec<WASM>,
}

/// A WASM expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    End(usize),
    If(Option<WASMType>),
    Then,
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

/// A WASM operator. Not comprehensive
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMOperator {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mult,
    /// Division
    Div,
    /// Remainder
    Rem,
    /// Shift right
    ShR,
    /// Shift left
    ShL,
    /// Equal
    Eq,
    /// Equal to zero
    EqZ,
    /// Not equal
    Ne,
    LtS,
    GtS,
    LeS,
    GeS,
    And,
    Or,
    Xor,
    Neg,
    /// Absolute value
    Abs,
}

/// A WASM type.
/// Either i32, i64, f32, or f64.
#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMType {
    i64,
    i32,
    f64,
    f32,
}

impl WASMType {
    pub fn size(&self) -> usize {
        match self {
            WASMType::i32 | WASMType::f32 => 1,
            WASMType::i64 | WASMType::f64 => 2,
        }
    }
}

/// Convert a module to LLR.
pub fn module_to_llr(
    module: &Node<Module>,
    context: &Context,
    cfg_map: &HashMap<Identifier, Cfg>,
) -> Result<WASMModule, GraceError> {
    let mut imports = vec![];
    let mut functions = vec![];
    let mut trait_implementations = vec![];

    for import in &module.data.imports {
        let typing_info = context.get_node_type(import.id)?;
        for value in &import.values {
            let (wasm_args, wasm_return) = match typing_info.resolve_attribute(value)? {
                // convert everything to WASMTypes
                Type::Function(ref args, ref kwargs, ref return_type) => {
                    let mut wasm_args: Vec<(String, WASMType)> = args
                        .iter()
                        .map(|(n, t)| (n.name.clone(), WASMType::from(t)))
                        .collect();
                    wasm_args.extend(
                        kwargs
                            .iter()
                            .map(|(n, t)| (n.name.clone(), WASMType::from(t))),
                    );
                    // wasm_args.extend(wasm_kwargs.into_iter());
                    let wasm_return = WASMType::from(&**return_type);
                    (wasm_args, wasm_return)
                }
                Type::Named(name) => {
                    let full_name = format!("{}.{}", import.string_ref(), name);
                    let actual_type = context.get_defined_type(&Identifier::from(full_name))?;
                    let (args, return_type) = actual_type.get_constructor_type();
                    let wasm_args = args
                        .iter()
                        .map(|(n, t)| (n.name.clone(), WASMType::from(t)))
                        .collect();
                    let wasm_return = WASMType::from(&return_type);
                    (wasm_args, wasm_return)
                }
                x => {
                    return Err(GraceError::compiler_error(format!(
                        "Wrong import type: {:?}. Expected {:?}. Should be handled by type_check",
                        x, typing_info
                    )))
                }
            };
            let joined_path = join(import.path.iter().map(|x| x.name.clone()), ".");
            let internal_name = format!(".{}.{}", joined_path, value);
            let wasm_import = WASMImport {
                path: joined_path,
                value: value.name.clone(),
                internal_name,
                params: wasm_args,
                return_type: wasm_return,
            };
            imports.push(wasm_import);
        }
    }

    for declaration in &module.data.functions {
        functions.push(handle_declaration(declaration, context, cfg_map)?);
    }

    for declaration in &module.data.structs {
        functions.push(handle_declaration(declaration, context, cfg_map)?);
    }

    for (trait_name, internal_type_name, function_decs) in &module.data.trait_implementations {
        for declaration in function_decs {
            let name_prefix = format!("{}.{}", trait_name, internal_type_name);
            let ambiguous_name_func =
                handle_trait_func_dec(declaration, &name_prefix, context, cfg_map)?;
            let full_name = format!(
                "{}.{}.{}",
                trait_name, internal_type_name, ambiguous_name_func.name
            );
            let full_name_func = WASMFunc {
                name: full_name,
                args: ambiguous_name_func.args,
                locals: ambiguous_name_func.locals,
                result: ambiguous_name_func.result,
                code: ambiguous_name_func.code,
            };
            trait_implementations.push(full_name_func);
        }
    }
    Ok(WASMModule {
        imports,
        functions,
        trait_implementations,
    })
}

/// Shared code to convert function and struct declarations to LLR
pub fn handle_declaration(
    declaration: &Node<Stmt>,
    context: &Context,
    cfg_map: &HashMap<Identifier, Cfg>,
) -> Result<WASMFunc, GraceError> {
    return match declaration.data {
        Stmt::FunctionDecStmt {
            ref name,
            ref args,
            ref kwargs,
            ref return_type,
            ..
        } => {
            let local_variables = context.get_contained_declarations(declaration.scope)?;
            let locals_with_wasm_types: Vec<(String, WASMType)> = local_variables
                .iter()
                .map(|(n, t)| (n.name.clone(), WASMType::from(t)))
                .collect();
            let cfg = cfg_map.get(name).ok_or(GraceError::compiler_error(format!(
                "No CFG found for function {}",
                name
            )))?;
            let block_llr = cfg.to_llr(context)?;
            let mut wasm_args: Vec<(String, WASMType)> = args
                .iter()
                .map(|(name, t)| (name.name.clone(), WASMType::from(t)))
                .collect();
            // Add kwargs
            wasm_args.append(
                &mut kwargs
                    .iter()
                    .map(|(name, t, _)| (name.name.clone(), WASMType::from(t)))
                    .collect(),
            );
            let wasm_func = WASMFunc {
                name: name.name.clone(),
                args: wasm_args,
                locals: locals_with_wasm_types,
                result: Option::<WASMType>::from(return_type),
                code: block_llr,
            };
            Ok(wasm_func)
        }
        Stmt::StructDec {
            ref name,
            ref fields,
        } => {
            let wasm_args: Vec<(String, WASMType)> = fields
                .iter()
                .map(|(name, t)| (name.name.clone(), WASMType::from(t)))
                .collect();
            let mut block_llr = vec![];
            let num_words: usize = wasm_args.iter().map(|(_, t)| t.size()).sum();
            block_llr.push(WASM::Const(format!("{}", num_words), WASMType::i32));
            block_llr.push(WASM::Call(".memory_management.alloc_words".to_string()));
            block_llr.push(WASM::Set(".x".to_string()));
            for (index, (field_name, _)) in fields.iter().enumerate() {
                block_llr.push(WASM::Get(".x".to_string()));
                block_llr.push(WASM::Const(format!("{}", 8 + index * 4), WASMType::i32));
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
                locals: vec![(".x".to_string(), WASMType::i32)],
                result: Some(WASMType::i32),
                code: block_llr,
            };
            Ok(wasm_func)
        }
        ref x => Err(GraceError::compiler_error(format!(
            "Got an unexpected declaration in a module: {:?}. Should not be allowed by the parser.",
            x
        ))),
    };
}

/// Convert a trait function to LLR.
pub fn handle_trait_func_dec(
    declaration: &Node<Stmt>,
    name_prefix: &String,
    context: &Context,
    cfg_map: &HashMap<Identifier, Cfg>,
) -> Result<WASMFunc, GraceError> {
    return match &declaration.data {
        Stmt::FunctionDecStmt {
            name,
            args,
            kwargs,
            return_type,
            ..
        } => {
            // Get local variables as WASM.
            let local_variables = context.get_contained_declarations(declaration.scope)?;
            let locals_with_wasm_types: Vec<(String, WASMType)> = local_variables
                .iter()
                .map(|(n, t)| (n.name.clone(), WASMType::from(t)))
                .collect();

            // Get function block LLR.
            let full_name = Identifier::from(format!("{}.{}", name_prefix, name.name));
            let cfg = cfg_map
                .get(&full_name)
                .ok_or(GraceError::compiler_error(format!(
                    "Could not find a CFG corresponding to the function {}.",
                    full_name
                )))?;
            let block_llr = cfg.to_llr(context)?;

            // Add args
            let mut wasm_args: Vec<(String, WASMType)> = args
                .iter()
                .map(|(name, t)| (name.name.clone(), WASMType::from(t)))
                .collect();

            // Add kwargs
            wasm_args.append(
                &mut kwargs
                    .iter()
                    .map(|(name, t, _)| (name.name.clone(), WASMType::from(t)))
                    .collect(),
            );
            let wasm_func = WASMFunc {
                name: name.name.clone(),
                args: wasm_args,
                locals: locals_with_wasm_types,
                result: Option::<WASMType>::from(return_type),
                code: block_llr,
            };
            Ok(wasm_func)
        }
        x => Err(GraceError::compiler_error(format!(
            "Got a non-function declaration in a trait: {:?}. Should not be allowed by the parser.",
            x
        ))),
    };
}

/// Convert a value to LLR.
pub trait ToLLR {
    fn to_llr(&self, context: &Context) -> Result<Vec<WASM>, GraceError>;
}

/// Convert a control flow graph to LLR.
/// Essentially just walks over the graph.
impl ToLLR for Cfg {
    fn to_llr(&self, context: &Context) -> Result<Vec<WASM>, GraceError> {
        let mut wasm = vec![];

        let mut unvisited = vec![self
            .entry_index
            .expect("No entry index found for a CFG. Should be impossible.")];

        while let Some(current_index) = unvisited.pop() {
            let node = self
                .graph
                .node_weight(current_index)
                .ok_or(GraceError::compiler_error(format!(
                    "Could not find a node with index {:?} in a CFG.",
                    current_index
                )))?;
            wasm.append(&mut node.to_llr(context)?);
            match node {
                CfgVertex::Block(_) | CfgVertex::Else | CfgVertex::End(_) | CfgVertex::Entry => {
                    let mut n_count = 0;
                    let edges = self.graph.edges_directed(current_index, Outgoing);
                    for edge in edges {
                        unvisited.push(edge.target());
                        n_count += 1;
                        assert_eq!(n_count, 1);
                    }
                }
                CfgVertex::IfStart(_, _) | CfgVertex::LoopStart(_) => {
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
                        None => {
                            return Err(GraceError::compiler_error(format!(
                            "No false block found for if statement {:?}. Should be created by cfg.",
                            self
                        )))
                        }
                    };

                    match true_block {
                        Some(x) => unvisited.push(x),
                        None => {
                            return Err(GraceError::compiler_error(format!(
                            "No true block found for if statement {:?}. Should be created by cfg.",
                            self
                        )))
                        }
                    }
                }
                CfgVertex::Break(_) | CfgVertex::Continue(_) => {
                    return Err(GraceError::compiler_error(
                        "Not implemented: break and continue".to_string(),
                    ));
                }
                CfgVertex::Exit => {}
            };
        }
        Ok(wasm)
    }
}

impl ToLLR for CfgVertex {
    fn to_llr(&self, context: &Context) -> Result<Vec<WASM>, GraceError> {
        Ok(match &self {
            CfgVertex::Block(statements) => {
                let mut wasm = vec![];
                for stmt in statements {
                    wasm.append(&mut stmt.to_llr(context)?);
                }
                wasm
            }
            CfgVertex::IfStart(expression, block_type) => {
                let wasm_type = Option::<WASMType>::from(block_type);
                let mut wasm = vec![WASM::If(wasm_type)];
                wasm = general_utils::join(wasm, expression.to_llr(context)?);
                wasm.push(WASM::Then);
                wasm
            }
            CfgVertex::LoopStart(expression) => {
                let mut wasm = vec![WASM::Block, WASM::Loop];
                wasm = general_utils::join(wasm, expression.to_llr(context)?);
                wasm.push(WASM::BranchIf(0));
                wasm
            }
            CfgVertex::Break(block) => {
                let mut wasm = vec![];
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context)?);
                }
                wasm.push(WASM::Branch(0));
                wasm
            }
            CfgVertex::Continue(block) => {
                let mut wasm = vec![];
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context)?);
                }
                // In WASM branching to 1 goes to the beginning of the loop.
                wasm.push(WASM::Branch(1));
                wasm
            }
            CfgVertex::Else => vec![WASM::Else],
            CfgVertex::End(k) => vec![WASM::End(*k)],
            CfgVertex::Entry | CfgVertex::Exit => vec![],
        })
    }
}

impl ToLLR for Node<CfgStmt> {
    fn to_llr(&self, context: &Context) -> Result<Vec<WASM>, GraceError> {
        Ok(match self.data {
            CfgStmt::Assignment {
                ref name,
                ref expression,
            } => {
                let mut expr_wasm = expression.to_llr(context)?;
                expr_wasm.push(WASM::Set(name.name.clone()));
                expr_wasm
            }
            CfgStmt::Return(ref val) => val.to_llr(context)?,
        })
    }
}

impl ToLLR for Node<Expr> {
    fn to_llr(&self, context: &Context) -> Result<Vec<WASM>, GraceError> {
        Ok(match &self.data {
            Expr::BinaryExpr {
                ref left,
                ref right,
                ref operator,
            } => {
                let mut llr = left.to_llr(context)?;
                llr.append(&mut right.to_llr(context)?);
                let left_id_type = context.get_node_type(left.id)?;
                let left_wasm_type = WASMType::from(&left_id_type);
                let right_id_type = context.get_node_type(right.id)?;
                let right_wasm_type = WASMType::from(&right_id_type);
                assert_eq!(left_wasm_type, right_wasm_type);
                llr.push(WASM::Operation(
                    WASMOperator::from(operator),
                    left_wasm_type,
                ));
                llr
            }
            Expr::FunctionCall {
                ref function,
                ref args,
                ..
            } => {
                let mut llr = vec![];
                for arg in args {
                    llr.append(&mut arg.to_llr(context)?);
                }
                match &function.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    }
                    Expr::ModuleAccess(_, ref path) => {
                        let func_name = join(path.iter().map(|x| x.name.clone()), ".");
                        llr.push(WASM::Call(format!(".{}", func_name)));
                    }
                    Expr::TraitAccess {
                        ref base,
                        ref trait_name,
                        ref attribute,
                    } => {
                        let base_type = context.get_node_type(base.id)?;
                        let full_func_name = format!(
                            "{}.{}.{}",
                            trait_name,
                            base_type.trait_impl_name(),
                            attribute
                        );
                        llr.push(WASM::Call(format!(".{}", full_func_name)));
                    }
                    x => {
                        return Err(GraceError::compiler_error(format!(
                            "FunctionCall ToLLR not implemented for base expression: {:?}",
                            x
                        )))
                    }
                }
                llr
            }
            Expr::StructLiteral {
                ref base,
                ref fields,
            } => {
                let mut llr = vec![];
                for field in fields {
                    llr.append(&mut field.to_llr(context)?);
                }
                match &base.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    }
                    Expr::ModuleAccess(_, ref path) => {
                        let func_name = join(path.iter().map(|x| x.name.clone()), ".");
                        llr.push(WASM::Call(format!(".{}", func_name)));
                    }
                    x => {
                        return Err(GraceError::compiler_error(format!(
                            "StructLiteral ToLLR not implemented for base expression: {:?}",
                            x
                        )))
                    }
                }
                llr
            }
            Expr::AttributeAccess {
                ref base,
                ref attribute,
            } => {
                let mut llr = vec![];
                // Get the address where the result of the base expression is stored.
                llr.append(&mut base.to_llr(context)?);
                let base_type = context.get_node_type(base.id)?;
                let new_llr = attr_access(&base_type, attribute, context)?;
                llr.extend(new_llr);
                llr
            }
            Expr::UnaryExpr {
                ref operator,
                ref operand,
            } => {
                let mut llr = operand.to_llr(context)?;
                match operator {
                    UnaryOperator::Negative => {
                        llr.push(WASM::Operation(WASMOperator::Neg, WASMType::i32));
                    }
                    UnaryOperator::BitNot => {
                        // Calculate bitnot by xor with full mask.
                        llr.push(WASM::Const("2147483647".to_string(), WASMType::i32));
                        llr.push(WASM::Operation(WASMOperator::Xor, WASMType::i32));
                    }
                    UnaryOperator::Not => {
                        llr.push(WASM::Operation(WASMOperator::EqZ, WASMType::i32));
                    }
                    UnaryOperator::Positive => {
                        llr.push(WASM::Operation(WASMOperator::Abs, WASMType::i32));
                    }
                };
                llr
            }
            Expr::Int(ref value) | Expr::Float(ref value) => {
                let id_type = context.get_node_type(self.id)?;
                let wasm_type = WASMType::from(&id_type);
                vec![WASM::Const(value.clone(), wasm_type)]
            }
            Expr::Bool(ref value) => match value {
                true => vec![WASM::Const("1".to_string(), WASMType::i32)],
                false => vec![WASM::Const("0".to_string(), WASMType::i32)],
            },
            Expr::IdentifierExpr(ref identifier) => vec![WASM::Get(identifier.name.clone())],
            Expr::VecLiteral(ref exprs) => {
                let t = context.get_node_type(self.id)?;
                let type_sizes = vec![t.size(); exprs.len()];
                // In WASM a vector is just a tuple of uniform type.
                store_tuple(exprs, &type_sizes, context)?
            }
            Expr::TupleLiteral(ref exprs) => {
                let t = context.get_node_type(self.id)?;
                let individual_types = match &t {
                    Type::Product(ref types) => types.clone(),
                    x => {
                        return Err(GraceError::compiler_error(format!(
                            "Mistyped tuple: {:?}. Should have been caught by type checking.",
                            x
                        )))
                    }
                };
                let type_sizes = individual_types
                    .iter()
                    .map(|x| x.size())
                    .collect::<Vec<_>>();
                store_tuple(exprs, &type_sizes, context)?
            }
            Expr::TraitAccess { .. } => {
                return Err(GraceError::compiler_error(
                    "TraitAccess not implemented".to_string(),
                ));
            }
            Expr::Index { .. } => {
                return Err(GraceError::compiler_error(
                    "Index not implemented".to_string(),
                ));
            }
            Expr::ModuleAccess { .. } => {
                return Err(GraceError::compiler_error(
                    "ModuleAccess not implemented".to_string(),
                ));
            }
            Expr::String { .. } => {
                return Err(GraceError::compiler_error(
                    "String not implemented".to_string(),
                ));
            }
        })
    }
}

/// Store a tuple/vector of data
///
/// # Arguments
/// * `exprs` - The expressions to store.
/// * `type_sizes` - The size of each type in the tuple. Constant for vectors.
/// * `context` - The typing context. Needed for recursive calls.
fn store_tuple(
    exprs: &[Node<Expr>],
    type_sizes: &[usize],
    context: &Context,
) -> Result<Vec<WASM>, GraceError> {
    let mut llr = vec![];
    // Total memory used by the tuple.
    let data_size: usize = type_sizes.iter().sum();
    let vector_size = data_size + HEADER_SIZE_WORDS;
    llr.push(WASM::from(vector_size));
    llr.push(WASM::Call(".memory_management.alloc_words".to_string()));
    // After calling alloc_words, the top of the stack is the address of the start of the tuple.

    for (i, expr) in exprs.iter().enumerate() {
        // Insert the code to calculate the address of the next element.
        llr.append(&mut expr.to_llr(context)?);
        // Store the value at the given address
        llr.push(WASM::Call(".memory_management.tee_memory".to_string()));
        // Put the size of the type on the stack.
        llr.push(WASM::Const(format!("{}", type_sizes[i] * 4), WASMType::i32));
        // Add the size of the type to the address.
        // The top of the stack is now the address of the next element.
        llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
    }
    // At the end of the for loop the top of the stack is the address of the last element + 1.
    // Put the start of the tuple on the stack
    llr.push(WASM::Const(format!("{}", data_size * 4), WASMType::i32));
    llr.push(WASM::Operation(WASMOperator::Sub, WASMType::i32));
    Ok(llr)
}

/// Recursively handle attribute accesses on named types.
fn attr_access(
    base_type: &Type,
    attribute: &Identifier,
    context: &Context,
) -> Result<Vec<WASM>, GraceError> {
    Ok(match base_type {
        Type::Record(ref names, ref fields) => {
            let mut llr = vec![];
            let attr_type = fields.get(attribute).unwrap();
            // Calculate the offset of `attribute` from the start of the expression result.
            let offset = calculate_offset(attribute, names, fields);
            llr.push(WASM::Const(format!("{}", offset), WASMType::i32));
            // Add the offset and the address
            llr.push(WASM::Operation(WASMOperator::Add, WASMType::i32));
            // Get the value at that address.
            llr.push(WASM::Load(WASMType::from(attr_type)));
            llr
        }
        Type::Named(ref name) => {
            let underlying_type = context.get_defined_type(name)?;
            attr_access(&underlying_type, attribute, context)?
        }
        x => {
            return Err(GraceError::compiler_error(format!(
                "Cannot access attribute of: {:?}. Should be a type error.",
                x
            )))
        }
    })
}

/// Calculate the offset of a field in a tuple.
///
/// # Arguments
/// * `name` - The name of the field to calculate the offset of.
/// * `order` - The order of the fields in the tuple.
/// * `type_map` - The types of the fields in the tuple.
fn calculate_offset(
    name: &Identifier,
    order: &Vec<Identifier>,
    type_map: &BTreeMap<Identifier, Type>,
) -> usize {
    let mut offset = HEADER_SIZE_BYTES;
    for val in order {
        if val == name {
            break;
        } else {
            // Size gives number of words, we want bytes.
            offset += type_map.get(val).unwrap().size() * 4;
        }
    }
    offset
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
                x => panic!("Tried to convert {:?} to WASM", x),
            }
        }
    }

    impl From<usize> for WASM {
        fn from(input: usize) -> Self {
            WASM::Const(format!("{}", input), WASMType::i32)
        }
    }

    impl From<i32> for WASM {
        fn from(input: i32) -> Self {
            WASM::Const(format!("{}", input), WASMType::i32)
        }
    }

    impl From<i64> for WASM {
        fn from(input: i64) -> Self {
            WASM::Const(format!("{}", input), WASMType::i64)
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
                x => panic!("Tried to convert {:?} to WASM", x),
            }
        }
    }

    impl From<&BinaryOperator> for WASMOperator {
        fn from(input: &BinaryOperator) -> Self {
            match input {
                BinaryOperator::Add => WASMOperator::Add,
                BinaryOperator::Sub => WASMOperator::Sub,
                BinaryOperator::Mult => WASMOperator::Mult,
                BinaryOperator::Div => WASMOperator::Div,
                BinaryOperator::And => WASMOperator::And,
                BinaryOperator::Or => WASMOperator::Or,
                BinaryOperator::Xor => WASMOperator::Xor,
                BinaryOperator::Equal => WASMOperator::Eq,
                BinaryOperator::Less => WASMOperator::LtS,
                BinaryOperator::Greater => WASMOperator::GtS,
                BinaryOperator::LessEqual => WASMOperator::LeS,
                BinaryOperator::GreaterEqual => WASMOperator::GeS,
                BinaryOperator::Unequal => WASMOperator::Ne,
                BinaryOperator::Mod => WASMOperator::Rem,
                BinaryOperator::BitShiftL => WASMOperator::ShL,
                BinaryOperator::BitShiftR => WASMOperator::ShR,
                BinaryOperator::BitAnd => panic!("No WASM operator for bitwise and"),
                BinaryOperator::BitOr => panic!("No WASM operator for bitwise or"),
                BinaryOperator::BitXor => panic!("No WASM operator for bitwise xor"),
                BinaryOperator::Exponent => panic!("No WASM operator for exponentiation"),
            }
        }
    }

    impl fmt::Display for WASMType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    WASMType::i32 => "i32",
                    WASMType::i64 => "i64",
                    WASMType::f32 => "f32",
                    WASMType::f64 => "f64",
                }
            )
        }
    }

    impl fmt::Display for WASMOperator {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    WASMOperator::Add => "add",
                    WASMOperator::Sub => "sub",
                    WASMOperator::Mult => "mul",
                    WASMOperator::Div => "div",
                    WASMOperator::Rem => "rem",
                    WASMOperator::ShL => "shift_left",
                    WASMOperator::ShR => "shift_right",
                    WASMOperator::Eq => "eq",
                    WASMOperator::Ne => "ne",
                    WASMOperator::GtS => "gt_s",
                    WASMOperator::LtS => "lt_s",
                    WASMOperator::GeS => "ge_s",
                    WASMOperator::LeS => "le_s",
                    WASMOperator::And => "and",
                    WASMOperator::Or => "or",
                    WASMOperator::Xor => "xor",
                    WASMOperator::EqZ => "eqz",
                    WASMOperator::Neg => "neg",
                    WASMOperator::Abs => "abs",
                }
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testing::minimal_examples::cfgs::minimal_cfg;

    use super::*;
    use std::fs::File;
    use std::io::Read;

    use cfg;
    use compiler_layers;
    use testing::minimal_examples;
    use testing::minimal_examples::cfgs as min_cfg;
    use testing::snippets;
    use type_checking::type_check::GetContext;

    /// Build a context and check the generated WASM.
    fn simple_llr_check<T: ToLLR>(value: T, expected: &[WASM]) {
        let context = Context::builtin();
        let res = value.to_llr(&context).unwrap();
        assert_eq!(res, expected);
    }

    #[cfg(test)]
    mod traits {
        use super::*;

        #[ignore]
        #[test]
        fn trait_impl_test() {
            let mut f = File::open("tests/test_data/trait_impl_test.gr").expect("File not found");
            let mut file_contents = String::new();
            f.read_to_string(&mut file_contents).unwrap();

            let (_, _, _, llr) = compiler_layers::to_llr(file_contents.as_bytes());
            let func_names: Vec<String> = llr.functions.iter().map(|x| x.name.clone()).collect();
            assert_eq!(
                func_names,
                vec!("call_trait_func".to_string(), "teststruct".to_string())
            );

            let trait_impl_names: Vec<String> = llr
                .trait_implementations
                .iter()
                .map(|x| x.name.clone())
                .collect();
            assert_eq!(
                trait_impl_names,
                vec!("testtrait.teststruct.baz".to_string())
            );
        }
    }

    #[cfg(test)]
    mod exprs {
        use super::*;

        fn check_expr(expr: Node<Expr>, expected_wasm: Vec<WASM>, context: Option<Context>) {
            let (result, context) = compiler_layers::ast_to_type_rewrites(expr, context);
            match result.to_llr(&context) {
                Ok(actual_wasm) => assert_eq!(actual_wasm, expected_wasm),
                Err(e) => panic!("{:?}", e),
            };
        }

        fn simple_check_expr(expr: Node<Expr>, expected_wasm: Vec<WASM>) {
            check_expr(expr, expected_wasm, None);
        }

        fn check_expr_seq(exprs: Vec<Node<Expr>>, expected_wasm: Vec<WASM>, context: Context) {
            let mut context = context;
            let mut actual_wasm = vec![];
            for expr in exprs {
                let (result, new_context) =
                    compiler_layers::ast_to_type_rewrites(expr, Some(context));
                context = new_context;
                actual_wasm.append(&mut result.to_llr(&context).unwrap());
            }
            assert_eq!(actual_wasm, expected_wasm);
        }

        #[test]
        fn llr_constants() {
            let expr: Node<Expr> = Node::from(0);
            simple_check_expr(expr, vec![WASM::Const("0".to_string(), WASMType::i32)]);
        }

        #[test]
        fn binary_ops() {
            let operand = Node::from(0);
            let comparisons = vec![BinaryOperator::Equal, BinaryOperator::Unequal];

            for comp in comparisons {
                let expr = Node::from(Expr::BinaryExpr {
                    operator: comp,
                    left: Box::new(operand.clone()),
                    right: Box::new(operand.clone()),
                });

                let wasm_op = WASMOperator::from(&comp);
                simple_check_expr(
                    expr,
                    vec![
                        WASM::Const("0".to_string(), WASMType::i32),
                        WASM::Const("0".to_string(), WASMType::i32),
                        WASM::Operation(wasm_op, WASMType::i32),
                    ],
                );
            }
        }

        #[test]
        fn function_call() {
            let func_call = minimal_examples::minimal_call();
            let (context, _) = minimal_examples::minimal_function_context();
            let expected = vec![WASM::Call("x".to_string())];
            check_expr(func_call, expected, Some(context));
        }

        #[test]
        fn struct_literal() {
            let expr = minimal_examples::minimal_struct_literal();
            let context = minimal_examples::minimal_struct_context();
            let expected = vec![WASM::Call("x".to_string())];
            check_expr(expr, expected, Some(context));
        }

        #[test]
        fn attribute_access() {
            let expr1 = minimal_examples::minimal_struct_literal();
            let expr2 = minimal_examples::minimal_attribute_access();
            let context = minimal_examples::minimal_struct_context();
            let expected = vec![
                WASM::Call("x".to_string()),
                WASM::Get("x".to_string()),
                WASM::Const("8".to_string(), WASMType::i32),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                WASM::Load(WASMType::i32),
            ];
            check_expr_seq(vec![expr1, expr2], expected, context);
        }

        #[test]
        fn unary_ops() {
            let expr = minimal_examples::minimal_unary();
            let expected = vec![
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Operation(WASMOperator::Neg, WASMType::i32),
            ];
            simple_check_expr(expr, expected);
        }

        #[test]
        fn tuple_literal() {
            let expr = minimal_examples::tuple_literal_numeric();
            let expected = vec![
                // Allocate memory
                WASM::from(5),
                WASM::Call(".memory_management.alloc_words".to_string()),
                // For rounds of storing 1
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                // Return to the start of the tuple
                WASM::from(12),
                WASM::Operation(WASMOperator::Sub, WASMType::i32),
            ];
            simple_check_expr(expr, expected);
        }

        #[test]
        fn vec_literal() {
            let expr = minimal_examples::vec_literal_numeric();
            let expected = vec![
                // Allocate memory
                WASM::from(5),
                WASM::Call(".memory_management.alloc_words".to_string()),
                // For rounds of storing 1
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                WASM::from(1),
                WASM::Call(".memory_management.tee_memory".to_string()),
                WASM::from(4),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
                // Return to the start of the tuple
                WASM::from(12),
                WASM::Operation(WASMOperator::Sub, WASMType::i32),
            ];
            simple_check_expr(expr, expected);
        }

        // fn
    }

    #[cfg(test)]
    mod vertex {
        use super::*;

        #[test]
        fn test_block() {
            let vertex = min_cfg::minimal_block();
            let expected = vec![
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_if_start() {
            let vertex = min_cfg::minimal_if_start();
            let expected = vec![
                WASM::If(Some(WASMType::i32)),
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Then,
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_loop_start() {
            let vertex = min_cfg::minimal_loop_start();
            let expected = vec![
                WASM::Block,
                WASM::Loop,
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::BranchIf(0),
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_break() {
            let vertex = min_cfg::minimal_break();
            let expected = vec![
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
                WASM::Branch(0),
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_continue() {
            let vertex = min_cfg::minimal_continue();
            let expected = vec![
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
                WASM::Branch(1),
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_else() {
            let vertex = min_cfg::minimal_else();
            let expected = vec![WASM::Else];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_end() {
            let vertex = min_cfg::minimal_end();
            let expected = vec![WASM::End(2)];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_entry() {
            let vertex = min_cfg::minimal_entry();
            let expected = vec![];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_exit() {
            let vertex = min_cfg::minimal_exit();
            let expected = vec![];
            simple_llr_check(vertex, &expected);
        }
    }

    #[cfg(test)]
    mod stmts {
        use super::*;
        #[test]
        fn test_assignment() {
            let vertex = min_cfg::minimal_cfg_assn();
            let expected = vec![
                WASM::Const("1".to_string(), WASMType::i32),
                WASM::Set("x".to_string()),
            ];
            simple_llr_check(vertex, &expected);
        }

        #[test]
        fn test_return() {
            let vertex = min_cfg::minimal_cfg_return();
            let expected = vec![WASM::Const("1".to_string(), WASMType::i32)];
            simple_llr_check(vertex, &expected);
        }
    }

    #[test]
    fn test_get_contained_declarations() {
        let (func_dec, context) = compiler_layers::to_context::<Node<Stmt>>(
            "fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes(),
        );
        let actual = context
            .get_contained_declarations(func_dec.scope)
            .unwrap()
            .iter()
            .cloned()
            .map(|x| x.0)
            .collect::<Vec<_>>();
        let expected = vec![Identifier::from("x")];
        assert_eq!(actual, expected);
    }

    #[test]
    fn refined_function_test() {
        let code = snippets::refined_fn();

        let (_module, _context, _cfg, wasm) = compiler_layers::to_llr(code.as_bytes());
        assert!(wasm.functions.len() == 1);
        let expected = WASMFunc {
            name: "require_ref".to_string(),
            args: vec![
                ("x".to_string(), WASMType::i32),
                ("y".to_string(), WASMType::i32),
            ],
            locals: vec![],
            result: Some(WASMType::i32),
            code: vec![
                WASM::Get("x".to_string()),
                WASM::Get("y".to_string()),
                WASM::Operation(WASMOperator::Add, WASMType::i32),
            ],
        };
        assert_eq!(wasm.functions[0], expected);
    }

    #[test]
    fn module_test() {
        let module = minimal_examples::minimal_module();
        let context = Context::builtin();
        let (context, _) = module.add_to_context(context).unwrap();
        let cfg_map = cfg::module_to_cfg(&module, &context);
        let res = module_to_llr(&module, &context, &cfg_map).unwrap();
        let func_wasm = WASMFunc {
            name: "x".to_string(),
            args: vec![],
            locals: vec![],
            result: Some(WASMType::i32),
            code: vec![WASM::from(1)],
        };
        let expected = WASMModule {
            imports: vec![],
            functions: vec![func_wasm],
            trait_implementations: vec![],
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn import_test() {
        let (module, context) = minimal_examples::minimal_import();
        let wasm = module_to_llr(&module, &context, &HashMap::new()).unwrap();
        let expected = WASMModule {
            imports: vec![WASMImport {
                path: "x".to_string(),
                value: "a".to_string(),
                internal_name: ".x.a".to_string(),
                params: vec![],
                return_type: WASMType::i32,
            }],
            functions: vec![],
            trait_implementations: vec![],
        };
        assert_eq!(wasm, expected);
    }

    #[test]
    fn cfg_to_llr_test() {
        let (cfg, context) = minimal_cfg();
        let result = cfg.to_llr(&context).unwrap();
        let expected = vec![
            WASM::from(1),
            WASM::from(1),
            WASM::Operation(WASMOperator::Add, WASMType::i32),
            WASM::Set("x".to_string()),
            WASM::from(1),
            WASM::from(1),
            WASM::Operation(WASMOperator::Add, WASMType::i32),
            WASM::Set("x".to_string()),
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn calculate_offset_test() {
        let order = vec![Identifier::from("x"), Identifier::from("y")];
        let mut type_map = BTreeMap::new();
        type_map.insert(Identifier::from("x"), Type::i32);
        type_map.insert(Identifier::from("y"), Type::i32);
        let offset = calculate_offset(&Identifier::from("x"), &order, &type_map);
        assert_eq!(offset, 8);
        let offset = calculate_offset(&Identifier::from("y"), &order, &type_map);
        assert_eq!(offset, 12);
    }
}
