# Scoping

## Information to Track
* What things are in scope (at every node)
* Where this name is declared (for all references)

## Architecture
file -> parser -> Module
Module -> create_scopes -> name_resolution

### Create scopes

Build scope recursively

Return a name resolution function (this is trivial, just search for ident in the scope):

    (scope, IdentifierExpr) -> declaration ASTNode (func dec, let stmt, comprehension)



### Name resolution



## Things that modify scope
* Let statements
* for loops (the loop variables)
* function declarations
* comprehensions
* import statements
* match expressions (not implemented in the parser yet)
* lambdas (ditto)

## Things to do when rewriting the parser
* Pass the scope, line number, and column number around along with the &[u8] input
* return child scope along with ast node
* Put a reference to the scope inside the ast node when it's made


## Name Resolution Options

### Node IDs (Definitely do)
Assign a unique ID to each node as it's created

* Requires us to edit Expr and Stmt
* IDs get stored but are invalidated when we load from a cache, have to be reassigned
* Has the advantage of generalizing if we need to incorporate mode data in the future

### Build scopes as we go
Make parent scope an input to each parser. Return the child scope.

* Tree node := (ASTNode, Scope). Edges are in ASTNode
* Seems like it would be more elegant to just put the scope in the node
