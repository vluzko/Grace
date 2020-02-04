use std::collections::HashMap;
use std::collections::BTreeMap;
use std::usize;
use std::ops::Add;
use std::convert::From;

use expression::*;
use general_utils;
use scoping;

/// Types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Type {
    i32,
    i64,
    f32,
    f64,
    ui32,
    ui64,
    string,
    boolean,
    empty,
    Sum(Vec<Type>),
    Product(Vec<Type>),
    Vector(Box<Type>),
    Function(Vec<(Identifier, Type)>, Box<Type>),
    Named(Identifier),
    Parameterized(Identifier, Vec<Type>),
    Record(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Module(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Undetermined
}

#[allow(non_snake_case)]
/// The signed integral types.
pub fn Signed<'a>() -> Type {
    Type::Sum(vec!(Type::i32, Type::i64))
}

#[allow(non_snake_case)]
/// The unsigned integral types.
pub fn Unsigned<'a>() -> Type {
    Type::Sum(vec!(Type::ui32))
}

#[allow(non_snake_case)]
/// The floating point types.
pub fn Integral<'a>() -> Type {
    return Signed() + Unsigned();
}

#[allow(non_snake_case)]
/// The floating point types.
pub fn FloatingPoint<'a>() -> Type {
    Type::Sum(vec!(Type::f32, Type::f64))
}

#[allow(non_snake_case)]
/// All numeric types.
pub fn Numeric<'a>() -> Type {
    return Integral() + FloatingPoint();
}

impl Type {

    /// Get the name of this type in WAST.
    pub fn wast_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "i32".to_string(),
            &Type::ui64 => "i64".to_string(),
            &Type::boolean => "i32".to_string(),
            &Type::empty => "".to_string(),
            &Type::Function(ref _args, ref ret) => {
                format!("(result {})", ret.wast_name())
            }
            &Type::Record(..) => {
                "i32".to_string()
            },
            _ => panic!()
        }
    }

    /// Get _s or _u for signed values, otherwise an empty string.
    pub fn sign(&self) -> String {
        match &self {
            &Type::i32 | &Type::i64 => "_s".to_string(),
            &Type::ui32 | &Type::ui64 => "_u".to_string(),
            _ => "".to_string()
        }
    }

    /// Merge two types if they're compatible.
    pub fn merge(&self, other: &Type) -> Type {
        if self == other {
            return self.clone()
        } else {
            return match self {
                Type::Sum(ref types) => {
                    match other {
                        Type::Sum(ref other_types) => {
                            Type::Sum(general_utils::vec_c_int(types, other_types))
                        }, 
                        x => {
                            if types.contains(&x) {
                                x.clone()
                            } else {
                                panic!()
                            }
                        }
                    }
                }, 
                Type::Undetermined => {
                    other.clone()
                },
                x => {
                    match other {
                        Type::Sum(ref other_types) => {
                            if other_types.contains(&x) {
                                x.clone()
                            } else {
                                panic!()
                            }
                        }, 
                        _ => panic!()
                    }
                }
            }
        }
    }

    pub fn size(&self) -> usize {
        return match self {
            Type::i32 => 1,
            Type::i64 => 2,
            Type::f32 => 1,
            Type::f64 => 2,
            Type::ui32 => 2,
            Type::boolean => 1,
            Type::string => 1,
            Type::Product(ref types) => types.iter().map(|x| x.size()).sum(),
            Type::Record(_, ref fields)  | Type::Module(_, ref fields) => fields.iter().map(|(_, t)| t.size()).sum(),
            _ => panic!()
        }
    }

    /// Return true if other can be restricted to self.
    pub fn super_type(&self, other: &Type) -> bool {
        return match self {
            Type::Sum(ref types) => {
                match other {
                    Type::Sum(ref type_vec) => general_utils::vec_subset(type_vec, types),
                    x => types.contains(x)
                }
            },
            _ => false
        };
    }

    pub fn resolve_attribute(&self, attribute: &Identifier) -> Type {
        return match self {
            Type::Record (_, attributes) | Type::Module(_, attributes) => {
                for (attr_name, attr_type) in attributes {
                    if attribute == attr_name {
                        return attr_type.clone();
                    }
                }
                panic!()
            },
            _ => panic!("The provided type doesn't have attributes.")
        };
    }

    pub fn flatten_to_record(idents: &Vec<Identifier>, base: BTreeMap<Identifier, Type>) -> Type {
        let mut rec = Type::Record(base.keys().map(|x| x.clone()).collect(), base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            let mut order = vec!();
            map.insert(ident.clone(), rec);
            order.push(ident.clone());
            rec = Type::Record(order, map);
        }
        return rec;
    }

    pub fn flatten_to_module(idents: &Vec<Identifier>, base: BTreeMap<Identifier, Type>) -> Type {
        let mut rec = Type::Module(base.keys().map(|x| x.clone()).collect(), base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            let mut order = vec!();
            map.insert(ident.clone(), rec);
            order.push(ident.clone());
            rec = Type::Module(order, map);
        }
        return rec;
    }

    pub fn resolve_nested_record(&self, idents: &Vec<Identifier>) -> Type {
        let mut t = self.clone();
        for ident in idents {
            t = t.resolve_attribute(ident);
        }
        return t.clone();
    }

    pub fn resolve_slice(&self, slices: &Vec<(Option<Type>, Option<Type>, Option<Type>)>) -> Type {
        return match self {
            Type::Vector(ref t) => match slices.get(0).unwrap() {
                (Some(_x), None, None) => (**t).clone(),
                _ => panic!()
            },
            _ => panic!()
        };
    }

    pub fn identifier_to_index(&self, ident: &Identifier) -> usize {
        return match self {
            Type::Record(ref order, ref fields) => {
                let mut words = 0;
                for i in order {
                    if i == ident {
                        break;
                    } else {
                        let size = fields.get(&i).unwrap().size();
                        words += size;
                    }
                }
                words
            },
            _ => panic!()
        };
    }
}

impl Add for Type {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        if self == other {
            return self.clone();
        } else {
            return match self {
                Type::Sum(ref types) => {
                    match other {
                        Type::Sum(ref other_types) => {
                            Type::Sum(general_utils::vec_c_union(types, other_types))
                        },
                        x => {
                            let mut new_vec = types.clone();
                            new_vec.push(x.clone());
                            Type::Sum(new_vec)
                        }
                    }
                },
                x => {
                    match other {
                        Type::Sum(ref other_types) => {
                            let mut new_vec = other_types.clone();
                            new_vec.push(x.clone());
                            Type::Sum(new_vec)
                        },
                        y => {
                            let new_vec = vec![x.clone(), y.clone()];
                            Type::Sum(new_vec)
                        }
                    }
                }
            };
        }
    }
}

impl <'a> From<&'a Identifier> for Type {
    fn from(input: &'a Identifier) -> Self {
        return match input.name.as_ref() {
            "i32" => Type::i32,
            "i64" => Type::i64,
            "f32" => Type::f32,
            "f64" => Type::f64,
            "ui32" => Type::ui32,
            "ui64" => Type::ui64,
            "boolean" => Type::boolean,
            "string" => Type::string,
            _ => Type::Named(input.clone())
        };
    }
}

impl From<Identifier> for Type {
    fn from(input: Identifier) -> Self {
        return match input.name.as_ref() {
            "i32" => Type::i32,
            "i64" => Type::i64,
            "f32" => Type::f32,
            "f64" => Type::f64,
            "ui32" => Type::ui32,
            "ui64" => Type::ui64,
            "boolean" => Type::boolean,
            "string" => Type::string,
            _ => Type::Named(input)
        };
    }
}

pub trait Typed<T> {
    fn type_based_rewrite(self, context: &mut scoping::Context2) -> T;

    fn resolve_types(&self, context: &scoping::Context, type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type);
}

impl Typed<scoping::CanModifyScope> for scoping::CanModifyScope {
    fn type_based_rewrite(self, _context: &mut scoping::Context2) -> scoping::CanModifyScope {
        panic!("Don't call type_based_rewrite on a CanModifyScope. Go through the AST.");
    }

    fn resolve_types(&self, context: &scoping::Context, type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        return match self {
            scoping::CanModifyScope::Statement(ref ptr, ref id) => {
                let stmt = unsafe {
                    &**ptr
                };
                // This can't be done in a match statement because Rust's borrow checker is wrong.
                if type_map.contains_key(id) {
                    let t = type_map.get(id).unwrap().clone();
                    (type_map, t)
                } else {
                    stmt.resolve_types(context, type_map)
                }
            },
            scoping::CanModifyScope::Expression(ref ptr, ref id) => {
                let expr = unsafe {
                    &**ptr
                };
                // This can't be done in a match statement because Rust's borrow checker is wrong.
                if type_map.contains_key(id) {
                    let t = type_map.get(id).unwrap().clone();
                    (type_map, t)
                } else {
                    expr.resolve_types(context, type_map)
                }
            },
            scoping::CanModifyScope::Argument(..) => {
                // Hack: assume all arguments are i32s for now
                (type_map, Type::i32)
            },
            scoping::CanModifyScope::ImportedModule(id) => {
                let func_type = type_map.get(id).unwrap().clone();
                (type_map, func_type)
            }
        };
    }
}

impl Typed<Node<Module>> for Node<Module> {
    fn type_based_rewrite(self, context: &mut scoping::Context2) -> Node<Module> {
        let new_decs = self.data.declarations.into_iter().map(|x| Box::new(x.type_based_rewrite(context))).collect();
        return Node{
            id: self.id,
            data: Module{declarations: new_decs, imports: self.data.imports},
            scope: self.scope
        };
    }

    fn resolve_types(&self, context: &scoping::Context, mut type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        for stmt in self.data.declarations.iter() {
            type_map = stmt.resolve_types(context, type_map).0;
        }
        type_map.insert(self.id, Type::empty);
        return (type_map, Type::empty);
    }
}

impl Typed<Node<Block>> for Node<Block> {
    fn type_based_rewrite(self, context: &mut scoping::Context2) -> Node<Block> {
        // let mut new_stmts = vec![];
        let new_stmts = self.data.statements.into_iter().map(|x| Box::new(x.type_based_rewrite(context))).collect();

        let new_block = Node{
            id: self.id,
            data: Block{statements: new_stmts},
            scope: self.scope
        };

        for stmt in &new_block.data.statements {
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} | Stmt::LetStmt{ref name, ..} | Stmt::StructDec{ref name, ..}  => {
                    context.append_declaration(self.scope, name, &stmt);
                },
                _ => {}
            };
        }
        // new_block.update_scope(context);
        return new_block;
    }

    fn resolve_types(&self, context: &scoping::Context, mut type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        let mut block_type = Type::Undetermined;
        for stmt in &self.data.statements {
            let res = stmt.resolve_types(context, type_map);
            type_map = res.0;
            let stmt_type = res.1;

            match stmt.data {
                Stmt::ReturnStmt(_) | Stmt::YieldStmt(_) => {
                    block_type = block_type.merge(&stmt_type);
                },
                _ => {}
            };
        }

        // Blocks with no return statement have the empty type.
        if block_type == Type::Undetermined {
            block_type = Type::empty;
        }

        type_map.insert(self.id, block_type.clone());

        return (type_map, block_type);
    }
}

impl Typed<Node<Stmt>> for Node<Stmt> {
    fn type_based_rewrite(self, context: &mut scoping::Context2) -> Node<Stmt> {
        let new_stmt = match self.data {
            Stmt::FunctionDecStmt {name, block, args, kwargs, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(context), name, args, kwargs, return_type}
            },
            Stmt::AssignmentStmt {mut expression, name} => {
                expression = expression.type_based_rewrite(context);
                Stmt::AssignmentStmt {name, expression: expression.type_based_rewrite(context)}
            },
            Stmt::IfStmt {condition, block, else_block} => {
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite(context))
                };
                Stmt::IfStmt {
                    condition: condition.type_based_rewrite(context), block: block.type_based_rewrite(context), else_block: new_else_block
                }
            },
            Stmt::LetStmt {name, type_annotation, expression} => {
                Stmt::LetStmt {name, type_annotation, expression: expression.type_based_rewrite(context)}
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(context), block: block.type_based_rewrite(context)}
            },
            Stmt::ReturnStmt (value) => {
                Stmt::ReturnStmt (value.type_based_rewrite(context))
            },
            _ => self.data
        };
        return Node {
            id: self.id, 
            data: new_stmt,
            scope: self.scope 
        };
    }

    fn resolve_types(&self, context: &scoping::Context, mut type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        return match self.data {
            Stmt::LetStmt{ref type_annotation, ref expression, ..} => {
                let (mut type_map, expr_type) = expression.resolve_types(context, type_map);

                // Type check
                match type_annotation {
                    Some(ta) => assert_eq!(ta, &expr_type),
                    None => {}
                };

                type_map.insert(self.id, expr_type.clone());
                (type_map, expr_type)
            },
            Stmt::AssignmentStmt{ref name, ref expression, ..} => {
                let (mut type_map, expr_type) = expression.resolve_types(context, type_map);

                // Type check
                let expected_type = type_map.get(&context.get_declaration(self.scope, name).unwrap().get_id()).unwrap();
                assert_eq!(expected_type, &expr_type);

                type_map.insert(self.id, expr_type.clone());
                (type_map, expr_type)                
            },
            Stmt::ReturnStmt(ref value) => {
                let (mut new_map, t) = value.resolve_types(context, type_map);
                new_map.insert(self.id, t.clone());
                (new_map, t)
            },
            Stmt::FunctionDecStmt{ref args, ref kwargs, ref block, ref return_type, ..} => {
                let (mut new_map, _) = block.resolve_types(context, type_map);

                // let total_args = args.len() + kwargs.len();
                // let argument_type = vec![Type::Undetermined; total_args];
                let mut arg_types = args.clone();
                for x in kwargs {
                    arg_types.push((x.0.clone(), x.1.clone()));
                }

                // TODO: Type check
                // assert_eq!(return_type.clone().unwrap(), t);
                let function_type = Type::Function(arg_types, Box::new(return_type.clone()));

                new_map.insert(self.id, function_type.clone());
                (new_map, function_type)
            },
            Stmt::WhileStmt{ref condition, ref block} => {
                let (new_map, _) = condition.resolve_types(context, type_map);
                let (mut final_map, t) = block.resolve_types(context, new_map);
                final_map.insert(self.id, t.clone());
                (final_map, t)
            }
            Stmt::IfStmt{ref condition, ref block, ref else_block} => {
                let (new_map, _) = condition.resolve_types(context, type_map);
                let (mut block_map, t) = block.resolve_types(context, new_map);
                block_map.insert(self.id, t.clone());

                block_map = match else_block {
                    Some(block) => block.resolve_types(context, block_map).0,
                    None => block_map
                };

                (block_map, t)
            }
            Stmt::StructDec{ref fields, ..} => {
                let mut order = vec!();
                let mut records = BTreeMap::new();
                for (n, t) in fields {
                    order.push(n.clone());
                    records.insert(n.clone(), t.clone());
                }
                let record = Type::Record(order, records);
                type_map.insert(self.id, record.clone());
                (type_map, record)
            },
            _ => panic!()
        };
    }
}

impl Typed<Node<Expr>> for Node<Expr> {
    fn type_based_rewrite(self, context: &mut scoping::Context2) -> Node<Expr> {
        let new_expr = match self.data {
            Expr::ComparisonExpr {left, right, operator} => {
                let left = Box::new(left.type_based_rewrite(context));
                let right = Box::new(right.type_based_rewrite(context));
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = context.g_type(left.id);
                let right_type = context.g_type(right.id);
                let new_left = left.type_based_rewrite(context);
                let new_right = right.type_based_rewrite(context);

                // TODO: Once typeclasses are implemented, call the typeclass method with
                // the operator, the left type, and the right type to figure out what all
                // the other types need to be.
                if Numeric().super_type(&left_type) && Numeric().super_type(&right_type) {
                    let merged_type = operator.get_return_types(&left_type, &right_type);
                    let converted_left = convert_expr(&new_left, &merged_type, &mut context.type_map);
                    let converted_right = convert_expr(&new_right, &merged_type, &mut context.type_map);
                    Expr::BinaryExpr{operator, left: Box::new(converted_left), right: Box::new(converted_right)}
                } else {
                    // TODO: Update this once typeclasses are implemented
                    assert_eq!(left_type, right_type);
                    Expr::BinaryExpr{operator, left: Box::new(new_left), right: Box::new(new_right)}
                }
            },
            Expr::FunctionCall {function, args, kwargs} => {
                let new_func_expr = Box::new(function.type_based_rewrite(context));
                let new_args = args.into_iter().map(|x| x.type_based_rewrite(context)).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite(context))).collect();
                Expr::FunctionCall {function: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            Expr::Int(_) | Expr::Float(_) => {
                let current_type = context.g_type(self.id);
                context.add_type(self.id, choose_return_type(&current_type));
                self.data
            },
            _ => self.data
        };

        return Node {
            id: self.id,
            data: new_expr,
            scope: self.scope
        };
    }

    fn resolve_types(&self, context: &scoping::Context, mut type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        return match &self.data {
            Expr::BinaryExpr{ref operator, ref left, ref right} => {
                let (left_map, left_type) = left.resolve_types(context, type_map);
                let (mut right_map, right_type) = right.resolve_types(context, left_map.clone());
                let return_type = operator.get_return_types(&left_type, &right_type);
                right_map.insert(self.id, return_type.clone());
                (right_map, return_type)
            },
            Expr::ComparisonExpr{ref left, ref right, ..} => {
                let (left_map, _) = left.resolve_types(context, type_map);
                let (mut right_map, _) = right.resolve_types(context, left_map);
                right_map.insert(self.id, Type::boolean);
                (right_map, Type::boolean)
            },
            Expr::UnaryExpr{ref operand, ..} => {
                let (mut new_map, new_type) = operand.resolve_types(context, type_map);
                new_map.insert(self.id, new_type.clone());
                (new_map, new_type)
            },
            Expr::FunctionCall{ref function, ref args, ref kwargs} => {
                let (mut new_map, t) = function.resolve_types(context, type_map);
                for arg in args {
                    new_map = arg.resolve_types(context, new_map).0;
                }

                for (_, value) in kwargs {
                    new_map = value.resolve_types(context, new_map).0;
                }
                new_map.insert(self.id, t.clone());
                (new_map, t)
            },
            Expr::AttributeAccess{ref base, ref attribute} => {
                let (mut new_map, base_type) = base.resolve_types(context, type_map);
                let attribute_type = base_type.resolve_attribute(attribute);
                new_map.insert(self.id, attribute_type.clone());
                (new_map, attribute_type)
            },
            Expr::Index{ref base, ref slices} => {
                let (mut new_map, base_type) = base.resolve_types(context, type_map);
                let mut slice_types = vec!();
                for (start, stop, step) in slices {
                    let a = match start {
                        Some(x) => {
                            let res = x.resolve_types(context, new_map);
                            new_map = res.0;
                            Some(res.1) 
                        },
                        None => None
                    };
                    let b = match stop {
                        Some(x) => {
                            let res = x.resolve_types(context, new_map);
                            new_map = res.0;
                            Some(res.1) 
                        },
                        None => None
                    };
                    let c = match step {
                        Some(x) => {
                            let res = x.resolve_types(context, new_map);
                            new_map = res.0;
                            Some(res.1) 
                        },
                        None => None
                    };
                    slice_types.push((a, b, c));
                }
                let result_type = base_type.resolve_slice(&slice_types);
                new_map.insert(self.id, result_type.clone());
                (new_map, result_type)
            },
            Expr::IdentifierExpr(ref name) => {
                let creation = context.get_declaration(self.scope, name).unwrap();
                let (mut new_map, t) = creation.resolve_types(context, type_map);
                new_map.insert(self.id, t.clone());
                (new_map, t)
            },
            Expr::String(_) => {
                type_map.insert(self.id, Type::string);
                (type_map, Type::string)
            },
            Expr::Float(_) => {
                type_map.insert(self.id, Type::f32);
                (type_map, Type::f32)
            },
            Expr::Bool(_) => {
                type_map.insert(self.id, Type::i32);
                (type_map, Type::i32)
            },
            Expr::Int(_) => {
                type_map.insert(self.id, Type::i32);
                (type_map, Type::i32)
            },
            Expr::VecLiteral(exprs) => {
                let mut vec_t = Type::Undetermined;
                for expr in exprs {
                    let res = expr.resolve_types(context, type_map);
                    type_map = res.0;
                    vec_t = vec_t.merge(&res.1);
                }
                type_map.insert(self.id, vec_t.clone());

                (type_map, Type::Vector(Box::new(vec_t)))
            },
            Expr::StructLiteral{ref base, ref fields} => {
                let (mut new_type_map, base_t) = base.resolve_types(context, type_map);
                for field in fields {
                    let res = field.resolve_types(context, new_type_map);
                    new_type_map = res.0;
                }
                new_type_map.insert(self.id, base_t.clone());
                (new_type_map, base_t.clone())
            },
            _ => panic!()
        };
    }
}

pub fn numeric_join(left_type: &Type, right_type: &Type) -> Type {
    println!("Left: {:?}", left_type);
    println!("Right: {:?}", right_type);
    // Topological ordering of numeric types.
    // i32, f32, i64, f64
    let order = vec![vec![Type::i32, Type::i64, Type::f64], vec![Type::f32, Type::f64], vec![Type::i64], vec![Type::f64]];
    let indices = hashmap!{Type::i32 => 0, Type::f32 => 1, Type::i64 => 2, Type::f64 => 3};
    let t1 = &order[*indices.get(left_type).unwrap()];
    let t2 = &order[*indices.get(right_type).unwrap()];
    let join = general_utils::vec_c_int(t1, t2);
    return join[0].clone();
}

// TODO: Return an option.
pub fn convert_expr(expr: &Node<Expr>, new_type: &Type, type_map: &mut HashMap<usize, Type>) -> Node<Expr> {
    let current_type = type_map.get(&expr.id).unwrap().clone();
    if &current_type == new_type {
        return expr.clone();
    } else {
        let operator = UnaryOperator::from(new_type);
        return expr.replace(Expr::UnaryExpr{
            operator, operand: Box::new(expr.clone())
        });
    }
}

pub fn choose_return_type(possible: &Type) -> Type {
    return match possible {
        Type::Sum(ref types) => {
            let mut i = 0;
            let mut min_size = usize::MAX;
            for (j, t) in types.iter().enumerate() {
                if t.size() < min_size {
                    i = j;
                    min_size = t.size();
                }
            }
            types[i].clone()
        },
        _ => possible.clone()
    }
}

impl BinaryOperator {

    pub fn get_return_types(&self, left: &Type, right: &Type) -> Type {
        //let mut intersection = HashSet::new();
        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => left.merge(right),
            BinaryOperator::Div => Type::f64,
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Type::boolean,
            _ => panic!()
        }
    }

    pub fn choose_return_type(&self, merged_type: &Type) -> Type {

        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => 
            choose_return_type(merged_type),
            BinaryOperator::Div => Type::f32,
            _ => panic!()   
        };
    }

    pub fn requires_sign(&self) -> bool {
        match self {
            BinaryOperator::Div => true,
            _ => false
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use compiler_layers;

    #[cfg(test)]
    mod types {

        use super::*;

        #[test]
        fn test_flatten() {
            let idents = vec!(Identifier::from("a"), Identifier::from("b"));
            let bottom_map = btreemap!{
                Identifier::from("c") => Type::Function(vec!((Identifier::from("x"), Type::i32)), Box::new(Type::i64)),
            };
            let record_type = Type::flatten_to_record(&idents, bottom_map.clone());
            let second_map = btreemap!{
                Identifier::from("b") => Type::Record(vec!(Identifier::from("c")), bottom_map),
            };
            assert_eq!(record_type, Type::Record(vec!(Identifier::from("b")), second_map));
        }
    }

    #[cfg(test)]
    mod expressions {
        use super::*;
    }

    #[cfg(test)]
    mod statements {
        use super::*;
        use compiler_layers;
        #[test]
        fn if_stmt_typing() {
            let (block, context) = compiler_layers::to_context::<Node<Block>>(if_stmt_fixture());
            let if_stmt = block.data.statements.get(2).unwrap();
            assert_eq!(context.type_map.get(&if_stmt.id).unwrap(), &Numeric());
        }
    }

    #[test]
    fn test_identifier_resolution() {
        let block_str = "let a = 1\nlet b = a";
        // let mut parsed = parser_utils::output(parser::block(block.as_bytes(), 0));
        // let (id, init) = scoping::initial_context();
        // let context = parsed.gen_scopes(id, &init);
        // let (types, _) = parsed.resolve_types(&context, HashMap::new());
        let (parsed, context) = compiler_layers::to_context::<Node<Block>>(block_str.as_bytes());
        assert_eq!(context.g_type(parsed.id), Type::empty);
        let id2 = parsed.data.statements[1].id;
        assert_eq!(context.g_type(id2), Numeric());
    }

    fn if_stmt_fixture<'a>() -> &'a [u8] {
        let if_stmt = r#"
        let a = 1
        let b = 1
        if false:
            return a
        else:
            return b"#;
        return if_stmt.as_bytes(); 
    }
}
