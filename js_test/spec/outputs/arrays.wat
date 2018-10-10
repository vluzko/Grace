(module
(import "memory_management" "alloc_words" (func $alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $free (param $a i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 2))

(func $create_array (param $number_of_elements i32) (param $element_size i32) (result i32)
    get_local $number_of_elements
    get_local $element_size
    i32.mul
    call $alloc_words
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
)
