(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "file_3" "func_3" (func $.file_3.func_3  (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $func_2  (result i32) 
i32.const 2
)
(export "func_2" (func $func_2))
)
