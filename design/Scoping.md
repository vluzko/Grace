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
