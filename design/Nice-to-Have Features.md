# Nice-to-Have Features


## Closures
Internally, a closure is a function and a context. Each of these must be represented by a pointer.

The function pointer is a pointer to some table, while the context pointer points to memory.
When we pass a closure around, we actually need to pass both pointers around.

When a closure is created, we need to:
* Rewrite it to be at the top level
* Rewrite it to expect an extra argument containing the context pointer
* Generate code to create and store the context
* Group the function pointer with the context pointer into a closure pointer.
* Replace references to the closure with references to a pointer to the closure.


## Lambda functions
Lambdas that don't enclose context are easy: we just generate a top level function and pass around a pointer to that function. If the lambda does enclose context then it's just an anonymous closure. The only difference is that we have to generate a name for it.

## Partial functions
Partial functions may not actually be achievable in WASM.

## Dynamic types
For dynamic types, we need:
* Run time access to the true type
* A way to turn the true type into function pointers to all the relevant functions
* A way to turn the true type into getters/setters for its contents.

It is not clear how achievable these are, or if they're worth it.
