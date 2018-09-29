(module (memory (export "mem") 2)

(func $inspect (param $loc i32) (result i32)
    get_local $loc
    i32.load
)(export "inspect" (func $inspect))

;;(func $set_mem (param $))


;; TODO: Inline
;; Create a chunk at the specified address with size $size and a pointer to $next_chunk.
(func $create_chunk (param $address i32) (param $next_chunk i32) (param $size i32) (result i32)
    get_local $address
    get_local $next_chunk
    i32.store               ;; Set the pointer to the next chunk

    get_local $address
    i32.const 4
    i32.add
    get_local $size
    i32.store
    get_local $address
)(export "create_chunk" (func $create_chunk))

;; TODO: Inline
;; Calculate the end of the given chunk
(func $calc_end (param $address i32) (result i32)
    get_local $address
    (i32.load (i32.add          ;; Load the size of the chunk
        (get_local $address)
        (i32.const 4)
    ))
    i32.const 4
    i32.mul                     ;; Multiply the chunk size by 4 to keep everything in words instead of bytes
    i32.add                     ;; Compute the end of next_chunk, minus overhead
    i32.const 8                 ;; We have 8 bytes of overhead from storing the linked list and the size of the chunk.
    i32.add                     ;; The real end of next_chunk
)(export "calc_end" (func $calc_end))

;; Allocate a chunk of memory for use as an array.
;; It's easy to get confused here.
;; *next_chunk* is the *address* of the next chunk. However the *value at that address* is the address of *next_next_chunk*.
(func $alloc2 (param $size i32) (result i32) (local $cur_chunk i32) (local $next_chunk i32) (local $new_chunk i32)
    i32.const 0
    i32.load
    tee_local $cur_chunk

    i32.eqz                                     ;; If the number of allocated chunks is 0, we assign the first chunk and return.
    if (result i32)
        i32.const 0
        i32.const 3
        i32.store                               ;; Set the first pointer to the first chunk

        i32.const 3
        i32.const 0
        get_local $size
        call $create_chunk                      ;; TODO: Figure out if the WASM compiler will inline function calls.
    else                                        ;; Otherwise we loop until we reach the last chunk, or the first gap between chunks large enough to store the new chunk.
        loop
            block
                get_local $cur_chunk
                i32.load
                i32.eqz
                br_if 0                         ;; End the loop if the pointer to the next chunk is null.

                get_local $cur_chunk
                i32.load                        ;; Get the address of the chunk n+1
                tee_local $next_chunk      ;; Store the address of next_next_chunk, and keep it on the stack.

                get_local $cur_chunk
                call $calc_end
                tee_local $new_chunk            ;; Store the end of the next chunk

                i32.sub                         ;; The amount of space between next_next_chunk and next_chunk.

                get_local $size
                i32.ge_u
                if                              ;; If the amount of space between chunks is larger than $size, we put the chunk in between these two.
                    get_local $cur_chunk
                    get_local $new_chunk
                    i32.store                   ;; Update next_chunk to point to the new chunk

                    get_local $new_chunk
                    get_local $next_chunk
                    get_local $size
                    call $create_chunk
                    return
                end

                ;; If there isn't enough space between the current chunks: loop.
                get_local $next_chunk
                set_local $cur_chunk

                br 1
            end
        end

        get_local $cur_chunk
        get_local $cur_chunk
        call $calc_end
        tee_local $new_chunk
        i32.store                               ;; Store a pointer to new_chunk in next_chunk

        get_local $new_chunk
        get_local $size
        i32.const 0
        call $create_chunk                      ;; Create the chunk and return its address
    end

)
(export "alloc2" (func $alloc2))
)

