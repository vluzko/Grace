# Wast

The existing docs for wast are pretty terrible. Here are some better ones.


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
Check if the top of the stack is 1, then br n if it is. Equivalent to:

    if
        br n+1
    end

### loop
Run a loop. Should contain a block, i.e.

    loop
        block


        end
    end

For some reason if you branch to the *block* the loop end, but branching to the *loop* continues the loop. I have no idea why this is the case.

    loop
        block
        ...
        br_if 0 ;; Branch to block / end the loop if the top of the stack is non-zero

        br 1 ;; Branch one level out / to the loop and continue the loop.

        end
    end

## Global variables


## Memory
Memory is accessed in *bytes*.

    i32.const n
    i32.load

Will get you the value stored at *bytes* n, n+1, n+2, n+3, interpreted as an integer.

Note on weird error messages: if you get something like " error: type mismatch in function, expected [] but got [i32, i32]
",
that can mean there was more than one thing left on the stack at the end. If the function is supposed to return,
there should be exactly one value of the right type on the stack at the end and nothing else.