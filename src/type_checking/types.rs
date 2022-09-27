use std::collections::{BTreeMap, HashMap, HashSet};

use expression::*;
use general_utils;
use grace_error::GraceError;

/// A Grace type
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
    self_type(Box<Type>),
    // A sum type, e.g. a union type
    Sum(Vec<Type>),
    // A product type, e.g. a tuple
    Product(Vec<Type>),
    // A vector type.
    Vector(Box<Type>),
    // A vector of argument and kwargument names and types, and the return type
    Function(Vec<(Identifier, Type)>, Vec<(Identifier, Type)>, Box<Type>),
    // A referenced to a named type.
    Named(Identifier),
    // Struct{name: Identifier, attributes: BTreeMap<Identifier, Type>, methods: BTreeMap<Identifier, Type>}
    Parameterized(Identifier, Vec<Type>),
    // Attribute names, attribute types
    Record(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Module(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Gradual(usize),
    Refinement(Box<Type>, Vec<Refinement>),
    Undetermined,
}

/// A refinement on a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Refinement {
    pub operator: ComparisonOperator,
    pub left: Box<Node<Expr>>,
    pub right: Box<Node<Expr>>,
}

/// A Grace trait / typeclass
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub name: Identifier,
    pub functions: HashMap<Identifier, Type>,
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
            &Type::Function(ref _args, ref _kwargs, ref ret) => {
                format!("(result {})", ret.wast_name())
            }
            &Type::Record(..) => "i32".to_string(),
            _ => panic!(),
        }
    }

    /// Get the name of this type in WAST.
    pub fn trait_impl_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "ui32".to_string(),
            &Type::ui64 => "ui64".to_string(),
            &Type::boolean => "boolean".to_string(),
            &Type::empty => panic!(),
            &Type::Function(..) => panic!(),
            &Type::Record(..) => panic!("TODO: handle trait_impl_name for Record."),
            &Type::Sum(..) => panic!("TODO: handle trait_impl_name for Sum."),
            &Type::Product(..) => panic!("TODO: handle trait_impl_name for Product."),
            &Type::Vector(..) => panic!("TODO: handle trait_impl_name for Vector."),
            &Type::Named(ref name) => name.name.clone(),
            &Type::Refinement(ref t, ref _refinements) => format!("{:?}", t),
            _ => panic!(),
        }
    }

    /// Get _s or _u for signed values, otherwise an empty string.
    pub fn sign(&self) -> String {
        match &self {
            &Type::i32 | &Type::i64 => "_s".to_string(),
            &Type::ui32 | &Type::ui64 => "_u".to_string(),
            _ => "".to_string(),
        }
    }

    /// Check if a type is a refinement type.
    pub fn is_simple(&self) -> bool {
        return match self {
            Type::Refinement(..) => false,
            _ => true,
        };
    }

    /// Check if a type is a primitive type
    pub fn is_primitive(&self) -> bool {
        return match self {
            Type::i32
            | Type::i64
            | Type::f32
            | Type::f64
            | Type::ui32
            | Type::ui64
            | Type::boolean => true,
            | Type::Refinement(base_t, ..) => base_t.is_primitive(),
            _ => false,
        };
    }

    /// Check if a type is a gradual type.
    /// Both pure gradual types and refinement types containing gradual types count.
    pub fn is_gradual(&self) -> bool {
        return match self {
            Type::Gradual(..) => true,
            Type::Refinement(ref inner_t, _) => inner_t.is_gradual(),
            _ => false,
        };
    }

    pub fn refined_equal(&self, other: &Type) -> bool {
        return match self {
            Type::Refinement(ref inner_t, ..) => {
                match other {
                    Type::Refinement(ref other_inner_t, ..) => inner_t.refined_equal(other_inner_t),
                    x => inner_t.refined_equal(x)
                }
            }
            _ => self == other,
        };
    }

    /// Merge two types if they're compatible.
    pub fn merge(&self, other: &Type) -> Type {
        if self == other {
            return self.clone();
        } else {
            return match self {
                Type::Sum(ref types) => match other {
                    Type::Sum(ref other_types) => {
                        Type::Sum(general_utils::vec_c_int(types, other_types))
                    }
                    x => {
                        if types.contains(&x) {
                            x.clone()
                        } else {
                            panic!()
                        }
                    }
                },
                Type::Refinement(ref base, ..) => other.merge(&base),
                Type::Undetermined => other.clone(),
                x => match other {
                    Type::Sum(ref other_types) => {
                        if other_types.contains(&x) {
                            x.clone()
                        } else {
                            panic!()
                        }
                    }
                    y => panic!("Type error. Tried to merge {:?} and {:?}", x, y),
                },
            };
        }
    }

    /// Check if it is possible to convert from one type to the other
    pub fn is_compatible(&self, other: &Type) -> bool {
        if self == other {
            return true;
        } else {
            return match &other {
                Type::Refinement(ref base, ..) => self.is_compatible(base),
                Type::Gradual(_) => true,
                _ => match self {
                    Type::Refinement(ref base, ..) => base.is_compatible(other),
                    Type::i32 => match other {
                        Type::i64 | Type::f64 => true,
                        _ => false,
                    },
                    Type::f32 => match other {
                        Type::f64 => true,
                        _ => false,
                    },
                    Type::Sum(ref types) => match other {
                        Type::Sum(_) => true,
                        x => types.contains(&x),
                    },
                    Type::Undetermined => true,
                    x => match other {
                        Type::Sum(ref other_types) => other_types.contains(&x),
                        _ => false,
                    },
                },
            };
        }
    }

    /// Check if the left type can be converted to the right type with a WASM operator.
    /// Only true for primitive numeric types.
    pub fn has_simple_conversion(&self, other: &Type) -> bool {
        return match self {
            Type::i32 => match other {
                Type::i64 | Type::f64 => true,
                _ => false,
            },
            Type::f32 => match other {
                Type::f64 => true,
                _ => false,
            },
            _ => false,
        };
    }

    /// The number of words required to store a type in WASM memory.
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
            Type::Record(_, ref fields) | Type::Module(_, ref fields) => {
                fields.iter().map(|(_, t)| t.size()).sum()
            }
            _ => panic!(),
        };
    }

    /// Return true if other can be restricted to self.
    pub fn super_type(&self, other: &Type) -> bool {
        return match self {
            Type::Sum(ref types) => match other {
                Type::Sum(ref type_vec) => general_utils::vec_subset(type_vec, types),
                x => types.contains(x),
            },
            _ => false,
        };
    }

    /// Check if the type has an attribute corresponding to the given identifier
    /// Only Records and Modules have attributes.
    pub fn has_attribute(&self, attribute: &Identifier) -> bool {
        return match self {
            Type::Record(_, attributes) | Type::Module(_, attributes) => {
                for (attr_name, _) in attributes {
                    if attribute == attr_name {
                        return true;
                    }
                }
                return false;
            }
            _ => false,
        };
    }

    pub fn resolve_attribute(&self, attribute: &Identifier) -> Result<Type, GraceError> {
        return match self {
            Type::Record(_, attributes) | Type::Module(_, attributes) => {
                let mut t = None;

                for (attr_name, attr_type) in attributes {
                    if attribute == attr_name {
                        t = Some(attr_type.clone());
                    }
                }
                match t {
                    Some(attr_type) => Ok(attr_type),
                    None => Err(GraceError::type_error(format!(
                        "Tried to access nonexistent attribute {:?} on type {:?}",
                        attribute, self
                    ))),
                }
            }
            _ => Err(GraceError::type_error(format!(
                "Tried to access attribute {:?} on non-record type {:?}",
                attribute, self
            ))),
        };
    }

    pub fn all_attributes(&self) -> HashSet<Identifier> {
        return match self {
            Type::Record(_, attributes) | Type::Module(_, attributes) => {
                attributes.keys().cloned().collect::<HashSet<Identifier>>()
            }
            _ => HashSet::new(),
        };
    }

    pub fn flatten_to_record(idents: &Vec<Identifier>, base: BTreeMap<Identifier, Type>) -> Type {
        let mut rec = Type::Record(base.keys().map(|x| x.clone()).collect(), base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            let mut order = vec![];
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
            let mut order = vec![];
            map.insert(ident.clone(), rec);
            order.push(ident.clone());
            rec = Type::Module(order, map);
        }
        return rec;
    }

    pub fn resolve_nested_record(&self, idents: &Vec<Identifier>) -> Result<Type, GraceError> {
        let mut t = self.clone();
        for ident in idents {
            t = t.resolve_attribute(ident)?;
        }
        return Ok(t.clone());
    }

    pub fn resolve_slice(&self, slices: &Vec<(Option<Type>, Option<Type>, Option<Type>)>) -> Type {
        return match self {
            Type::Vector(ref t) => match slices.get(0).unwrap() {
                (Some(_x), None, None) => (**t).clone(),
                _ => panic!(),
            },
            _ => panic!(),
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
            }
            _ => panic!(),
        };
    }

    pub fn get_constructor_type(&self) -> (Vec<(Identifier, Type)>, Type) {
        return match &self {
            Type::Record(_, ref fields) => {
                let args: Vec<(Identifier, Type)> = fields.clone().into_iter().collect();
                (args, Type::i32)
            }
            _ => panic!(),
        };
    }

    /// Add a constraint if the type is a refinement. Do nothing otherwise.
    pub fn add_constraint(&self, name: &Identifier, expr: &Node<Expr>) -> Type {
        return match self {
            Type::Refinement(ref base, ref constraints) => {
                let mut new_constraints = constraints.clone();
                new_constraints.push(Refinement {
                    operator: ComparisonOperator::Equal,
                    left: Box::new(Node {
                        id: general_utils::get_next_id(),
                        start_line: expr.start_line,
                        start_col: expr.start_col,
                        end_line: expr.end_line,
                        end_col: expr.end_col,
                        scope: expr.scope,
                        data: Expr::from(name.clone()),
                    }),
                    right: Box::new(expr.clone()),
                });

                Type::Refinement(base.clone(), new_constraints)
            }
            x => x.clone(),
        };
    }
}

/// Constructors
impl Type {
    /// Construct a module type from a type map.
    pub fn module_from_map(map: BTreeMap<Identifier, Type>) -> Type {
        let keys = map.keys().cloned().collect();
        let module_type = Type::Module(keys, map);
        return module_type;
    }

    /// Construct an argumentless function type.
    pub fn func_no_args(return_type: Type) -> Type {
        return Type::Function(vec![], vec![], Box::new(return_type));
    }
}
