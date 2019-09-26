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
    Function(Vec<Type>, Box<Type>),
    Named(Identifier),
    Parameterized(Identifier, Vec<Type>),
    Record(BTreeMap<Identifier, Type>),
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
                            return Type::Sum(general_utils::vec_c_int(types, other_types))
                        }, 
                        _ => panic!()
                    }
                }, 
                Type::Undetermined => {
                    other.clone()
                },
                _ => panic!()
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
            Type::Record (attributes) => {
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
        let mut rec = Type::Record(base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            map.insert(ident.clone(), rec);
            rec = Type::Record(map);
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
    fn type_based_rewrite(self, context: &mut scoping::Context, type_map: &mut HashMap<usize, Type>) -> T;

    fn resolve_types(&self, context: &scoping::Context, type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type);
}

impl Typed<scoping::CanModifyScope> for scoping::CanModifyScope {
    fn type_based_rewrite(self, _context: &mut scoping::Context, _type_map: &mut HashMap<usize, Type>) -> scoping::CanModifyScope {
        panic!("Don't call type_based_rewrite on a CanModifyScope. Go through the AST.");
    }

    fn resolve_types(&self, context: &scoping::Context, type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        return match self {
            scoping::CanModifyScope::Statement(ref ptr) => {
                let stmt = unsafe {
                    &**ptr
                };
                // This can't be done in a match statement because Rust's borrow checker is wrong.
                if type_map.contains_key(&stmt.id) {
                    let t = type_map.get(&stmt.id).unwrap().clone();
                    (type_map, t)
                } else {
                    stmt.resolve_types(context, type_map)
                }
            },
            scoping::CanModifyScope::Expression(ref ptr) => {
                let expr = unsafe {
                    &**ptr
                };
                // This can't be done in a match statement because Rust's borrow checker is wrong.
                if type_map.contains_key(&expr.id) {
                    let t = type_map.get(&expr.id).unwrap().clone();
                    (type_map, t)
                } else {
                    expr.resolve_types(context, type_map)
                }
            },
            scoping::CanModifyScope::Argument(..) => {
                // Hack: assume all arguments are i32s for now
                (type_map, Type::i32)
                // let arg = unsafe {
                //     &**ptr
                // };
                // // This can't be done in a match statement because Rust's borrow checker is wrong.
                // if type_map.contains_key(&arg.id) {
                //     let t = type_map.get(&arg.id).unwrap().clone();
                //     (type_map, t)
                // } else {
                //     arg.resolve_types(context, type_map)
                // }                
            },
            scoping::CanModifyScope::ImportedFunction(id) => {
                let func_type = type_map.get(id).unwrap().clone();
                (type_map, func_type)
            },
            _ => panic!()
        };
    }
}

impl Typed<Node<Module>> for Node<Module> {
    fn type_based_rewrite(self, context: &mut scoping::Context, type_map: &mut HashMap<usize, Type>) -> Node<Module> {
        let new_decs = c![Box::new(x.type_based_rewrite(context, type_map)), for x in self.data.declarations];
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
    fn type_based_rewrite(self, context: &mut scoping::Context, type_map: &mut HashMap<usize, Type>) -> Node<Block> {
        // let mut new_stmts = vec![];
        let new_stmts = c![Box::new(x.type_based_rewrite(context, type_map)), for x in self.data.statements];

        let new_block = Node{
            id: self.id,
            data: Block{statements: new_stmts},
            scope: self.scope
        };

        new_block.update_scope(context);
        return new_block;
    }

    fn resolve_types(&self, context: &scoping::Context, mut type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        let mut block_type = Type::Undetermined;
        for stmt in self.data.statements.iter() {
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
    fn type_based_rewrite(self, context: &mut scoping::Context, type_map: &mut HashMap<usize, Type>) -> Node<Stmt> {
        let new_stmt = match self.data {
            Stmt::FunctionDecStmt {name, block, args, vararg, kwargs, varkwarg, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(context, type_map), name, args, vararg, kwargs, varkwarg, return_type}
            },
            Stmt::AssignmentStmt {mut expression, name, operator} => {
                expression = expression.type_based_rewrite(context, type_map);
                Stmt::AssignmentStmt {name, operator, expression: expression.type_based_rewrite(context, type_map)}
            },
            Stmt::IfStmt {condition, block, elifs,  else_block} => {
                let new_elifs =  c![(elif.0.type_based_rewrite(context, type_map), elif.1.type_based_rewrite(context, type_map)), for elif in elifs];
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite(context, type_map))
                };
                Stmt::IfStmt {condition: condition.type_based_rewrite(context, type_map), block: block.type_based_rewrite(context, type_map), elifs: new_elifs, else_block: new_else_block}
            },
            Stmt::LetStmt {typed_name, expression} => {
                Stmt::LetStmt {typed_name, expression: expression.type_based_rewrite(context, type_map)}
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(context, type_map), block: block.type_based_rewrite(context, type_map)}
            },
            Stmt::ReturnStmt (value) => {
                Stmt::ReturnStmt (value.type_based_rewrite(context, type_map))
            },
            _ => self.data
        };
        return Node {
            id: self.id, 
            data: new_stmt,
            scope: self.scope 
        };
    }

    fn resolve_types(&self, context: &scoping::Context, type_map: HashMap<usize, Type>) -> (HashMap<usize, Type>, Type) {
        return match self.data {
            Stmt::LetStmt{ref expression, ..} => {
                let (mut type_map, expr_type) = expression.resolve_types(context, type_map);
                type_map.insert(self.id, expr_type.clone());
                (type_map, expr_type)
            },
            Stmt::AssignmentStmt{ref expression, ..} => {
                let (mut type_map, expr_type) = expression.resolve_types(context, type_map);
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
                let mut arg_types = c![x.1.clone(), for x in args];
                arg_types.append(&mut c![x.1.clone(), for x in kwargs]);

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
            Stmt::IfStmt{ref condition, ref block, ref elifs, ref else_block} => {
                let (new_map, _) = condition.resolve_types(context, type_map);
                let (mut block_map, t) = block.resolve_types(context, new_map);
                block_map.insert(self.id, t.clone());

                // Elifs
                for (expr, block) in elifs {
                    let expr_map = expr.resolve_types(context, block_map).0;
                    block_map = block.resolve_types(context, expr_map).0;
                }

                block_map = match else_block {
                    Some(block) => block.resolve_types(context, block_map).0,
                    None => block_map
                };

                (block_map, t)
            }
            _ => panic!()
        };
    }
}

impl Typed<Node<Expr>> for Node<Expr> {
    fn type_based_rewrite(self, context: &mut scoping::Context, type_map: &mut HashMap<usize, Type>) -> Node<Expr> {
        let new_expr = match self.data {
            Expr::ComparisonExpr {left, right, operator} => {
                let left = Box::new(left.type_based_rewrite(context, type_map));
                let right = Box::new(right.type_based_rewrite(context, type_map));
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = type_map.get(&left.id).unwrap().clone();
                let right_type = type_map.get(&right.id).unwrap().clone();
                let new_left = left.type_based_rewrite(context, type_map);
                let new_right = right.type_based_rewrite(context, type_map);

                // TODO: Once typeclasses are implemented, call the typeclass method with
                // the operator, the left type, and the right type to figure out what all
                // the other types need to be.
                if Numeric().super_type(&left_type) && Numeric().super_type(&right_type) {
                    let merged_type = operator.get_return_types(&left_type, &right_type);
                    let converted_left = convert_expr(&new_left, &merged_type, type_map);
                    let converted_right = convert_expr(&new_right, &merged_type, type_map);
                    Expr::BinaryExpr{operator, left: Box::new(converted_left), right: Box::new(converted_right)}
                } else {
                    // TODO: Update this once typeclasses are implemented
                    assert_eq!(left_type, right_type);
                    Expr::BinaryExpr{operator, left: Box::new(new_left), right: Box::new(new_right)}
                }
            },
            Expr::FunctionCall {function, args, kwargs} => {
                let new_func_expr = Box::new(function.type_based_rewrite(context, type_map));
                let new_args = args.into_iter().map(|x| x.type_based_rewrite(context, type_map)).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite(context, type_map))).collect();
                Expr::FunctionCall {function: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            Expr::Int(_) | Expr::Float(_) => {
                let current_type = type_map.get(&self.id).unwrap().clone();
                type_map.insert(self.id, choose_return_type(&current_type));
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
            _ => panic!()
        };
    }
}

pub fn numeric_join(left_type: &Type, right_type: &Type) -> Type {
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
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => numeric_join(left, right),
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
                Identifier::from("c") => Type::Function(vec!(Type::i32), Box::new(Type::i64)),
            };
            let record_type = Type::flatten_to_record(&idents, bottom_map.clone());
            let second_map = btreemap!{
                Identifier::from("b") => Type::Record(bottom_map),
            };
            assert_eq!(Type::Record(second_map), record_type);
        }
    }

    #[cfg(test)]
    mod expressions {
        use super::*;

        #[test]
        fn binary_expr_types() {
            let (expr, context) = compiler_layers::to_scopes::<Node<Expr>>("5 + 6".as_bytes());
            let (types, _) = expr.resolve_types(&context, HashMap::new());
            assert_eq!(types.get(&expr.id).unwrap(), &Type::i32);
        }
    }

    #[cfg(test)]
    mod statements {
        use super::*;
        use compiler_layers;
        #[test]
        fn if_stmt_typing() {
            let (block, _, type_map) = compiler_layers::to_types::<Node<Block>>(if_stmt_fixture());
            let if_stmt = block.data.statements.get(2).unwrap();
            assert_eq!(type_map.get(&if_stmt.id).unwrap(), &Type::i32);
        }
    }

    #[test]
    fn test_identifier_resolution() {
        let block_str = "let a = 1\nlet b = a";
        // let mut parsed = parser_utils::output(parser::block(block.as_bytes(), 0));
        // let (id, init) = scoping::initial_context();
        // let context = parsed.gen_scopes(id, &init);
        // let (types, _) = parsed.resolve_types(&context, HashMap::new());
        let (parsed, _, types) = compiler_layers::to_types::<Node<Block>>(block_str.as_bytes());
        assert_eq!(types.get(&parsed.id), Some(&Type::empty));
        let id2 = parsed.data.statements[1].id;
        assert_eq!(types.get(&id2), Some(&Type::i32));
    }

    fn if_stmt_fixture<'a>() -> &'a [u8] {
        let if_stmt = r#"
        let a = 1
        let b = 1
        if 0:
            return a
        else:
            return b"#;
        return if_stmt.as_bytes(); 
    }
}
