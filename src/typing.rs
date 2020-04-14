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
                        y => panic!("Type error. Tried to merge {:?} and {:?}", x, y)
                    }
                }
            }
        }
    }

    /// Check if it is possible to convert from one type to the other
    pub fn is_compatible(&self, other: &Type) -> bool {
        if self == other {
            return true
        } else {
            return match self {
                Type::i32 => {
                    match other {
                        Type::i64 | Type::f64 => true,
                        _ => false
                    }
                },
                Type::f32 => {
                    match other {
                        Type::f64 => true,
                        _ => false
                    }
                },
                Type::Sum(ref types) => {
                    match other {
                        Type::Sum(_) => true, 
                        x => types.contains(&x)
                    }
                }, 
                Type::Undetermined => true,
                x => {
                    match other {
                        Type::Sum(ref other_types) => other_types.contains(&x),
                        _ => false
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
            Type::Vector(ref t) => t.size(),
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
                let mut t = None;
                
                for (attr_name, attr_type) in attributes {
                    if attribute == attr_name {
                        t = Some(attr_type.clone());
                    }
                }
                match t {
                    Some(attr_type) => attr_type,
                    None => panic!()
                }
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

    pub fn get_constructor_type(&self) -> (Vec<(Identifier, Type)>, Type) {
        return match &self {
            Type::Record(_, ref fields) => {
                let args: Vec<(Identifier, Type)> = fields.clone().into_iter().collect();
                (args, Type::i32)
            },
            _ => panic!()
        }
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
    fn type_based_rewrite(self, context: &mut scoping::Context) -> T;
}

impl Typed<Node<Module>> for Node<Module> {
    fn type_based_rewrite(self, context: &mut scoping::Context) -> Node<Module> {
        let new_decs = self.data.declarations.into_iter().map(|x| Box::new(x.type_based_rewrite(context))).collect();
        return Node{
            id: self.id,
            data: Module{declarations: new_decs, imports: self.data.imports},
            scope: self.scope
        };
    }
}

impl Typed<Node<Block>> for Node<Block> {
    fn type_based_rewrite(self, context: &mut scoping::Context) -> Node<Block> {
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
}

impl Typed<Node<Stmt>> for Node<Stmt> {
    fn type_based_rewrite(self, context: &mut scoping::Context) -> Node<Stmt> {
        let new_stmt = match self.data {
            Stmt::LetStmt {name, type_annotation, expression} => {
                let returned_type = context.get_node_type(expression.id);
                let base = expression.type_based_rewrite(context);
                let expr = match &type_annotation {
                    Some(x) => get_convert_expr(&returned_type, &x, base, context),
                    None => base
                };
                Stmt::LetStmt {name, type_annotation, expression: expr}
            },
            Stmt::AssignmentStmt {mut expression, name} => {
                let expected_type = context.get_node_type(self.id);
                let actual_type = context.get_node_type(expression.id);
                let base = expression.type_based_rewrite(context);
                let expr = get_convert_expr(&actual_type, &expected_type, base, context);
                Stmt::AssignmentStmt {name, expression: expr}
            },
            Stmt::FunctionDecStmt {name, block, args, kwargs, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(context), name, args, kwargs, return_type}
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
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(context), block: block.type_based_rewrite(context)}
            },
            Stmt::ReturnStmt (value) => {
                let exp_type = context.get_type(self.scope, &Identifier::from("$ret"));
                let ret_type = context.get_node_type(value.id);
                let base = value.type_based_rewrite(context);
                assert!(ret_type.is_compatible(&exp_type));

                let expr = match ret_type == exp_type {
                    true => base,
                    false => get_convert_expr(&ret_type, &exp_type, base, context)
                };
                Stmt::ReturnStmt(expr)
            },
            _ => self.data
        };
        return Node {
            id: self.id, 
            data: new_stmt,
            scope: self.scope 
        };
    }
}

impl Typed<Node<Expr>> for Node<Expr> {
    fn type_based_rewrite(self, context: &mut scoping::Context) -> Node<Expr> {
        let new_expr = match self.data {
            Expr::ComparisonExpr {left, right, operator} => {
                let left = Box::new(left.type_based_rewrite(context));
                let right = Box::new(right.type_based_rewrite(context));
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = context.get_node_type(left.id);
                let right_type = context.get_node_type(right.id);
                let new_left = left.type_based_rewrite(context);
                let new_right = right.type_based_rewrite(context);

                // TODO: Once typeclasses are implemented, call the typeclass method with
                // the operator, the left type, and the right type to figure out what all
                // the other types need to be.
                if Numeric().super_type(&left_type) && Numeric().super_type(&right_type) {
                    let merged_type = operator.get_return_types(&left_type, &right_type);
                    let converted_left = get_convert_expr(&left_type, &merged_type, new_left, context);
                    let converted_right = get_convert_expr(&right_type, &merged_type, new_right, context);
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
                let current_type = context.get_node_type(self.id);
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

fn get_convert_expr(from: &Type, to: &Type, base: Node<Expr>, context: &mut scoping::Context) -> Node<Expr> {
    return match from == to {
        false => {
            let operator = UnaryOperator::cast(&from, &to);
            let raw_expr = Expr::UnaryExpr{operator: operator, operand: Box::new(base)};
            let new_node = Node::from(raw_expr);
            context.add_type(new_node.id, to.clone());
            new_node
        },
        true => base
    };
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


impl UnaryOperator {
    fn cast(from: &Type, to: &Type) -> UnaryOperator {
        let possible = from.is_compatible(to);

        if possible {
            return UnaryOperator::Convert(from.clone(), to.clone());
        } else {
            panic!("Cannot cast from {:?} to {:?}", from, to);
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
            let (fun, context) = compiler_layers::to_context::<Node<Stmt>>(if_stmt_fixture());
            match fun.data {
                Stmt::FunctionDecStmt{ref block, ..} => {
                    let if_stmt = block.data.statements.get(2).unwrap();
                    assert_eq!(context.type_map.get(&if_stmt.id).unwrap(), &Type::i32);
                },
                _ => panic!()
            };
            
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
        assert_eq!(context.get_node_type(parsed.id), Type::empty);
        let id2 = parsed.data.statements[1].id;
        assert_eq!(context.get_node_type(id2), Type::i32);
    }

    fn if_stmt_fixture<'a>() -> &'a [u8] {
        let if_stmt = r#"fn a() -> i32:
            let a = 1
            let b = 1
            if false:
                return a
            else::
                return b"#;
        return if_stmt.as_bytes(); 
    }
}
