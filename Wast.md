# Wast

The existing docs for wast are pretty terrible. Here are some better ones.

## br
br n branches n levels out. br 0 branches to the current block/loop/etc. br 1 branches to the block/loop containing that.

## br_if n
Check if the top of the stack is 1, then br n if it is. Equivalent to:

    if
        br n+1
    end

## loop
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
