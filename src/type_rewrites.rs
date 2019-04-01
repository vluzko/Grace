use expression::*;


pub trait TypeRewrite<T> {
    fn type_based_rewrite(self) -> T;
}

impl <T> TypeRewrite<Node<T>> for Node<T>
    where T: TypeRewrite<T> {
    fn type_based_rewrite(self) -> Node<T> {
        let new_data = self.data.type_based_rewrite();
        return Node{
            id: self.id,
            data: new_data,
            scope: self.scope
        };
    }
}

impl TypeRewrite<Module> for Module {
    fn type_based_rewrite(self) -> Module {
        let new_decs = c![x.type_based_rewrite(), for x in self.declarations];
        return Module{ declarations: new_decs};
    }
}

impl TypeRewrite<Block> for Block {
    fn type_based_rewrite(self) -> Block {
        let new_stmts = c![x.type_based_rewrite(), for x in self.statements];
        return Block{statements: new_stmts};
    }
}

impl TypeRewrite<Stmt> for Stmt {
    fn type_based_rewrite(self) -> Stmt {
        
        let new_stmt = match self {
            Stmt::FunctionDecStmt {name, block, args, vararg, kwargs, varkwarg, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(), name, args, vararg, kwargs, varkwarg, return_type}
            },
            Stmt::AssignmentStmt {mut expression, name, operator} => {
                expression = expression.type_based_rewrite();
                Stmt::AssignmentStmt {name, operator, expression: expression.type_based_rewrite()}
            },
            Stmt::IfStmt {condition, block, elifs,  else_block} => {
                let new_elifs =  c![(elif.0.type_based_rewrite(), elif.1.type_based_rewrite()), for elif in elifs];
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite())
                };
                Stmt::IfStmt {condition: condition.type_based_rewrite(), block: block.type_based_rewrite(), elifs: new_elifs, else_block: new_else_block}
            },
            Stmt::ReturnStmt (value) => {
                Stmt::ReturnStmt (value.type_based_rewrite())
            },
            Stmt::LetStmt {typed_name, expression} => {
                Stmt::LetStmt {typed_name, expression: expression.type_based_rewrite()}
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(), block: block.type_based_rewrite()}
            },
            _ => self
        };
        return new_stmt;
    }
}
impl TypeRewrite<Expr> for Expr {
    fn type_based_rewrite(self) -> Expr {
        let new_expr = match self {
            Expr::ComparisonExpr {mut left, mut right, operator} => {
                let left = Box::new(left.type_based_rewrite());
                let right = Box::new(right.type_based_rewrite());
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = &left.get_type();
                let right_type = &right.get_type();
                let return_type = &operator.get_return_type(left_type, right_type);
                let new_left = if left_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    let left_expr = Expr::UnaryExpr { operator: conversion_op, operand: left.clone()};
                    let node = left.replace(left_expr);
                    Box::new(node)
                } else {
                    left
                };
                let new_right = if right_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    let right_expr = Expr::UnaryExpr { operator: conversion_op, operand: right.clone()};
                    let node = right.replace(right_expr);
                    Box::new(node)
                } else {
                    right
                };
                Expr::BinaryExpr {operator: operator, left: new_left, right: new_right}
            },
            Expr::FunctionCall {function, args, kwargs} => {
                let new_func_expr = Box::new(function.type_based_rewrite());
                let new_args = args.into_iter().map(|x| x.type_based_rewrite()).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite())).collect();
                Expr::FunctionCall {function: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            _ => self
        };

        return new_expr;
    }
}

#[cfg(test)]
mod test {

}
