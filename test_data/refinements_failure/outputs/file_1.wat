(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $greater_than_zero (param $x i32) (result i32) 
get_local $x
)
(export "greater_than_zero" (func $greater_than_zero))

(func $call_require  (result i32) (local $a i32)
i32.const -1
set_local $a
get_local $a
call $greater_than_zero
)
(export "call_require" (func $call_require))
)
