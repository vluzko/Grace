(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

;; Create an array
;; Args
;;      number_of_elements (i32)        - The length of the array in elements
;;      element_size (i32)              - The size of each element in words
(func $create_array (param $number_of_elements i32) (param $element_size i32) (result i32)
    get_local $number_of_elements
    get_local $element_size
    i32.mul
    call $.memory_management.alloc_words
)(export "create_array" (func $create_array))

(func $get_value (param $array_start i32) (param $index i32) (param $element_size i32) (result i32)
    get_local $element_size
    get_local $index
    i32.mul

    get_local $array_start
    i32.add
    i32.load
)(export "get_value" (func $get_value))

;; Set a value in an array
;; Args
;;      value (i32)         - The value to set
;;      array_start (i32)   - The index (in bytes) that the array *actually* starts at (*not* the index where the chunk containing it starts).
;;      index (i32)         - The index *in the array* at which to set the element. Should be in terms of *elements of the array*, not bytes, words, or anything else.
;;      element_size (i32)  - The size in bytes of an element of the array.
(func $set_value (param $value i32) (param $array_start i32) (param $index i32) (param $element_size i32)
    get_local $element_size
    get_local $index
    i32.mul

    get_local $array_start
    i32.add

    get_local $value
    i32.store

)(export "set_value" (func $set_value))

;; Delete an array.
;; Args
;;      array_index (i32): Pointer to the array (not the metadata of the chunk containing the array)
(func $delete (param $array_index i32)(result i32)
    get_local $array_index
    call $.memory_management.free_chunk 
)(export "delete" (func $delete))

;; Resize an array.
;; Args:
;;      array_index (i32):  Pointer to the data segment of the chunk containing the array to resize
;;      number_of_elements (i32):   The number of elements in the new array
;;      element_size (i32):         The size of a single element (in words)
;;
;; Returns:
;;     Pointer to the data segment of the resized array.
(func $resize (param $array_index i32) (param $number_of_elements i32) (param $element_size i32) (result i32)
(local $cur_size i32) (local $new_size i32) (local $ptr_to_size i32) (local $ptr_to_meta i32) (local $ptr_to_new i32) (local $ptr_to_next i32)
    ;; Calculate the new size.
    get_local $number_of_elements
    get_local $element_size
    i32.mul
    i32.const 4
    i32.mul                         ;; go from words to bytes
    set_local $new_size

    ;; Get the current size of the array.
    (i32.sub (get_local $array_index) (i32.const 4))
    tee_local $ptr_to_size
    i32.load
    tee_local $cur_size


    get_local $new_size
    i32.gt_u
    if (result i32)
        ;; If new_size < cur_size, change the size and otherwise don't modify the array.
        get_local $ptr_to_size
        get_local $new_size
        i32.store

        get_local $array_index
    else
        get_local $cur_size
        get_local $new_size
        i32.eq

        ;; If new_size == cur_size, stop
        if (result i32)
            get_local $array_index
        else
            get_local $ptr_to_size
            i32.const 4
            i32.sub
            tee_local $ptr_to_meta
            i32.load
            tee_local $ptr_to_next

            ;; Handle the case where the array being resized is in the last chunk.
            i32.eqz
            if
                memory.size
                i32.const 65536
                i32.mul
                set_local $ptr_to_next
            end

            get_local $ptr_to_next

            ;; Calculate size of space between current chunk and next chunk
            get_local $array_index
            i32.sub

            get_local $new_size
            i32.ge_u
            ;; If new_size > cur_size and there's room to expand
            if (result i32)
                get_local $ptr_to_size
                get_local $new_size
                i32.store

                get_local $array_index
            else
                ;; If new_size > cur_size and there isn't enough room to expand, free the current array, create a new one, and copy

                get_local $array_index
                call $.memory_management.free_chunk        ;; Free the current chunk
                drop

                get_local $array_index  ;; Load onto the stack

                get_local $new_size
                call $.memory_management.alloc_words       ;; Create a new chunk
                tee_local $ptr_to_new

                get_local $new_size
                call $.memory_management.copy_many
                drop
                get_local $ptr_to_new
            end
        end
    end
)(export "resize" (func $resize))
)
