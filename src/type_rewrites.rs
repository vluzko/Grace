use expression::*;


pub trait TypeRewrite<T> {
    fn type_based_rewrite(self) -> T;
}

impl TypeRewrite<Module> for Module {
    fn type_based_rewrite(self) -> Module {
        panic!()
    }
}

impl TypeRewrite<Block> for Block {
    fn type_based_rewrite(self) -> Block {
        panic!()
    }
}

impl TypeRewrite<Stmt> for Stmt {
    fn type_based_rewrite(self) -> Stmt {
        let new_stmt = match self {
            Stmt::FunctionDecStmt {name, args, keyword_args, vararg, varkwarg, body, return_type} => {
                Stmt::FunctionDecStmt {name, args, keyword_args, vararg, varkwarg, body: body.type_based_rewrite(), return_type}
            },
            Stmt::AssignmentStmt {identifier, operator, expression} => {
                Stmt::AssignmentStmt {identifier, operator, expression: expression.type_based_rewrite()}
            },
            // Only handles if x {foo}, no elifs or else
            Stmt::IfStmt {condition, main_block, elifs,  else_block} => {
                let new_elifs =  c![(elif.0.type_based_rewrite(), elif.1.type_based_rewrite()), for elif in elifs];
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite())
                };
                Stmt::IfStmt {condition: condition.type_based_rewrite(), main_block: main_block.type_based_rewrite(), elifs: new_elifs, else_block: new_else_block}
            },
            Stmt::ReturnStmt {value} => {
                Stmt::ReturnStmt {value: value.type_based_rewrite()}
            },
            Stmt::LetStmt {value_name, value} => {
                Stmt::LetStmt {value_name, value: value.type_based_rewrite()}
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(), block: block.type_based_rewrite()}
            },
            _ => self
        };
        new_stmt
    }
}

impl TypeRewrite<Expr> for Expr {
    fn type_based_rewrite(self) -> Expr {
        let new_expr = match self {
            Expr::ComparisonExpr { operator, left, right} => {
                Expr::ComparisonExpr {operator, left: Box::new(left.type_based_rewrite()), right: Box::new(right.type_based_rewrite())}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = &left.get_type();
                let right_type = &right.get_type();
                let return_type = &operator.get_return_type(left_type, right_type);
                let new_left = if left_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    Box::new(Expr::UnaryExpr {operator: conversion_op, operand: left})
                } else {
                    left
                };
                let new_right= if right_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    Box::new(Expr::UnaryExpr {operator: conversion_op, operand: right})
                } else {
                    right
                };
                Expr::BinaryExpr {operator: operator, left: new_left, right: new_right}
            },
            Expr::FunctionCall {func_expr, args, kwargs} => {
                let new_func_expr = Box::new(func_expr.type_based_rewrite());
                let new_args = args.into_iter().map(|x| x.type_based_rewrite()).collect();
                let new_kwargs = match kwargs {
                    Some(x) => Some(x.into_iter().map(|x| (x.0, x.1.type_based_rewrite())).collect()),
                    None => None
                };
                Expr::FunctionCall {func_expr: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            _ => self
        };

        return new_expr;
    }
}
