(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "gradual_binary_ops" "call_gradual" (func $.gradual_binary_ops.call_gradual (param $i i32) (param $a i32) (param $b i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $gradual_add (param $x i32) (param $y i32) (result i32) 
i32.const 0
get_local $x
get_local $y
call $.gradual_binary_ops.call_gradual
)
(export "gradual_add" (func $gradual_add))
)
