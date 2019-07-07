# WebAssembly Notes

The documentation for WebAssembly is woefully inadequate, and also the implementations are all out of sync with the main spec. Here are some notes.
* The Mozilla documentation is not in sync with the Node implementation. Node does not, for instance, have `instantiateStreaming`.


## Instructions
All convert instructions are out of sync. The spec lists them as `{type1}.{convert_op}_{type2}`. However they're all of the form `{type1}.{convert_op}/{type2}`.

Example. In the spec we have:

    f64.convert_s_i32

to convert an `i32` to an `f64`. But the actual wabt implementation has:

    f64.conver_s/i32

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
