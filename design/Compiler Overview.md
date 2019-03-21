# Compiler Overview

## Overview

    File -> Node
    Node.name_resolution -> Result<Map<Node, Scope>, Error>

## Parser
Information required:

* Line number range
* Column number range
* Node
* Scope?


## Scoping and Name Resolution

    get_scope: Node -> Scope
    scope_map: Map<Node, Scope>

## Typechecking

    get_type: Identifier -> Scope -> Node -> Type
    get_expected_type: Node -> Type

## Rewrites

    type_rewrite: T:Node -> T

## Bytecode Generation

    generate: T:Node -> String

## Debugging
## Scoping
* The raw AST nodes are all contained within IdNodes, which carry a unique ui64 identifier and a scope. IdNodes are initialized (in the parsing step) to have empty scopes.
* Scope is determined recursively: each node receives its parent scope, adds its own declarations to the scope, and passes that to its children. Each node returns a new IdNode with the proper scope
* Scope checking is also recursive: each node is passed its parent scope, checks that all of its references are resolvable, and then passes its scope to its children.

## Type Inference
* 

## Future Optimizations
* Determine scope at parse time?

### Reference counted pointers
There are a few places where we copy data (particularly for caching). What we really want to do is use reference counting pointers. This should significantly reduce memory overhead.
* e.g. when doing type checking we need to go from identifier -> declaring node. We can do this naively by recalculating it every time, or by cacheing the result (requires copying the declaring node and uses more memory), or by using reference counted pointers (probably the most performant solution)
* Also used when multiple nodes share a scope (e.g. nested binary exprs). Currently we copy these, obviously a reference counted pointer to the parent scope is better.
