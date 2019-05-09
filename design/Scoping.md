# Scoping

## Pointer architecture

### Problems
* The current architecture doesn't work because everything is stack allocated and addresses change
* Scope contains raw pointers. When we rewrite the tree, those addresses change and the scope is no longer valid.

### Solution 1: Box everything
* Might not actually work? Box *should* put stuff on the heap and give it a stable address though.
* Then we can use raw pointers to the Box or the contents of the Box, we *think*

#### Handling rewrites
* When a scope is *rewritten*, *if* it is modifies a scope, we then... rewrite the scope????
* Oh no, now every time we have to rewrite the entire thing!!!
* Only workable if we make everything mutable. *hiss*

### Solution 2: Rc pointers
* Same problem as Box everything.

### Solution 3: Big ass tables
* Two arrays / hashmaps containing all the scopes and all the nodes, indexed/mapped to by their IDs
* Maps between nodes/scopes are just the corresponding IDs

#### Handling rewrites
* Do the rewrite, update the table. Find the corresponding scope in its table, rewrite it. Preserve IDs.

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
