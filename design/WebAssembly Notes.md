# WebAssembly Notes

The documentation for WebAssembly is woefully inadequate, and also the implementations are all out of sync with the main spec. Here are some notes.
* The Mozilla documentation is not in sync with the Node implementation. Node does not, for instance, have `instantiateStreaming`.


## Instructions
All convert instructions are out of sync. The spec lists them as `{type1}.{convert_op}_{type2}`. However they're all of the form `{type1}.{convert_op}/{type2}`.

Example. In the spec we have:

    f64.convert_s_i32

to convert an `i32` to an `f64`. But the actual wabt implementation has:

    f64.convert_s/i32

## Loading WebAssembly
There are two steps to instantiating a WebAssembly module: compilation and instantiation. In order to actually use it you need to compile and then instantiate.

When you instantiate a WebAssembly module in Javascript, you must pass an imports object if the WebAssembly module in question has any imports. This import object must have the following format:

    {
        name_of_module: {
            name_of_function: function
        }
    }

As an example, suppose module `a` has been compiled and needs to import module `b`, and that `b` imports nothing. We have already loaded `b` into a WebAssembly module object. Then the following will instantiate `a`:

    WebAssembly.instantiate(a, {
        b: b.instance.exports
    })


## Tables

Tables are declared with:

    (table $table_name size_of_table anyfunc)

`anyfunc` is the signature allowed in the table. `anyfunc` is currently the only one supported by WebAssembly.

Example: `(table $tb 2 anyfunc)` creates a table of size 2 that can handle any function.

Functions are added to a table with:

    (elem (i32.const starting_index) $func_1 $func_2 ...)

Example: `(elem (i32.const 0) $foo $bar)` will add `foo` and `bar` to the table, at indices 0 and 1 respectively. Note that you do *not* need to name the table when you add the functions to it. (I believe that currently it's impossible to have more than one table available anyway, so there's no point to being able to name it).

Calling a function pointer:

    call_indirect $function_signature ;; with the function pointer on the top of the stack.

Example:

    (type $generic_binary (func (param $a i32) (param $b i32) (result i32)))
    i32.const 0
    call_indirect (type $generic_binary)

will call the function at index 0 of the table, assuming that function has the $generic_binary type signature.