# WAST

The existing docs for WAST are, we think, pretty terrible. Here are some better ones.


## Instructions

### Binary operators
The top of the stack is always the right operand, the next element is always the left operand.

    i32.const 10
    i32.const 5
    i32.sub

is `10 - 5`. The same is true for all other arithmetic, logic, and comparison operators.

### br
br n branches n levels out. br 0 branches to the current block/loop/etc. br 1 branches to the block/loop containing that.

### br_if n
Check if the top of the stack is 1, then br n if it is.

### loop
Run a loop. Should contain a block, i.e.

    loop
        block


        end
    end

If you branch to the block the loop ends (because branching to a block goes to the end of the block). If you branch to the loop you branch to the beginning of the loop.

## Global variables

## Memory
Memory is accessed in *bytes*.

    i32.const n
    i32.load

Will get you the value stored at *bytes* n, n+1, n+2, n+3, interpreted as an integer.

## Error messages
Note on weird error messages: if you get something like " error: type mismatch in function, expected [] but got [i32, i32]
",
that can mean there was more than one thing left on the stack at the end. If the function is supposed to return,
there should be exactly one value of the right type on the stack at the end and nothing else.

## Imports / Exports

### Importing memory
If you have memory `mem` defined in module `b` with initial size `n`, then:

    (import "b" "mem" (memory (;0;) n))

`n` should be an int literal, in case that isn't obvious.

### Importing a function
To import a function `a` of type `x y z -> w` from a module `b`, add these lines to the top of your module:

    type $type_name (func (param x y z) (result w)))
    (import "b" "a" (func $new_name_of_func (type $type_name)))

Then just call `$new_name_of_func` normally.

where `number` is actually a number (e.g. 0) that hasn't already been used in another type statement.

~~It looks like there's some way to define type and the imported function on the same line, but we haven't figured it out yet.~~ Do this:

    (import "b" "a" (func $new_name_of_func (param $param_name param_type)* (result result_type)*))

### Passing Imports
When you load a WASM module in Javascript with `WebAssembly.instantiate`, you must pass an imports object to `module.instantiate`. For example: suppose wasm_module 1 is expecting to import function `a` from module `b`. Then your imports object should look like:

    imports = {
        "b": {
            "a": Function
        }
    }

Accomplish this by instantiating module `b` first, then create an object containing it. Note that you *don't* want `module.instantiates(b)` as the contents of imports, you want `module.instantiates(b).instance.exports`, because that's where `a` is actually defined.

## Javascript Interaction
When calling a function from Javascript, it will pass 0 for any unfulfilled parameters. It will *not* break if you don't pass enough parameters.
