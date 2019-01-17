use expression::*;


pub trait TypeRewrite<T> {
    fn type_based_rewrite(self) -> T;
}

impl TypeRewrite<Module> for Module {
    fn type_based_rewrite(self) -> Module {
        let new_decs = c![x.type_based_rewrite(), for x in self.declarations];
        return Module{id: 0, declarations: new_decs};
    }
}

impl TypeRewrite<Block> for Block {
    fn type_based_rewrite(self) -> Block {
        let new_stmts = c![x.type_based_rewrite(), for x in self.statements];
        return Block{id:0, statements: new_stmts};
    }
}

impl TypeRewrite<Stmt> for Stmt {
    fn type_based_rewrite(self) -> Stmt {
        
        let new_stmt = match self {
            Stmt::FunctionDecStmt {id, name, body, args, vararg, keyword_args, varkwarg, return_type} => {
                Stmt::FunctionDecStmt {body: body.type_based_rewrite(), id, name, args, vararg, keyword_args, varkwarg, return_type}
            },
            Stmt::AssignmentStmt {id, mut expression, identifier, operator} => {
                expression = expression.type_based_rewrite();
                Stmt::AssignmentStmt {id, identifier, operator, expression: expression.type_based_rewrite()}
            },
            Stmt::IfStmt {id, condition, main_block, elifs,  else_block} => {
                let new_elifs =  c![(elif.0.type_based_rewrite(), elif.1.type_based_rewrite()), for elif in elifs];
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite())
                };
                Stmt::IfStmt {id, condition: condition.type_based_rewrite(), main_block: main_block.type_based_rewrite(), elifs: new_elifs, else_block: new_else_block}
            },
            Stmt::ReturnStmt {id, value} => {
                Stmt::ReturnStmt {id, value: value.type_based_rewrite()}
            },
            Stmt::LetStmt {id, value_name, value} => {
                Stmt::LetStmt {id, value_name, value: value.type_based_rewrite()}
            },
            Stmt::WhileStmt {id, condition, block} => {
                Stmt::WhileStmt {id, condition: condition.type_based_rewrite(), block: block.type_based_rewrite()}
            },
            _ => self
        };
        return new_stmt;
    }
}

impl TypeRewrite<Expr> for Expr {
    fn type_based_rewrite(self) -> Expr {


        let new_expr = match self {
            Expr::ComparisonExpr {mut left, mut right, id, operator} => {
                let left = Box::new(left.type_based_rewrite());
                let right = Box::new(right.type_based_rewrite());
                Expr::ComparisonExpr {left, right, id, operator}
            },
            Expr::BinaryExpr {id, operator, left, right} => {
                let left_type = &left.get_type();
                let right_type = &right.get_type();
                let return_type = &operator.get_return_type(left_type, right_type);
                let new_left = if left_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    Box::new(Expr::UnaryExpr {id: 0, operator: conversion_op, operand: left})
                } else {
                    left
                };
                let new_right= if right_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    Box::new(Expr::UnaryExpr {id: 0, operator: conversion_op, operand: right})
                } else {
                    right
                };
                Expr::BinaryExpr {id, operator: operator, left: new_left, right: new_right}
            },
            Expr::FunctionCall {id, func_expr, args, kwargs} => {
                let new_func_expr = Box::new(func_expr.type_based_rewrite());
                let new_args = args.into_iter().map(|x| x.type_based_rewrite()).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite())).collect();
                Expr::FunctionCall {id, func_expr: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            _ => self
        };

        return new_expr;
    }
}


#[cfg(test)]
mod test {

}
