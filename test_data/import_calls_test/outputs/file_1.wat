(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))
(import "file_2" "A" (func $.file_2.A i32))
(import "file_2" "func_2" (func $.file_2.func_2 (result i32)))
(func $func_1  (result i32) (local $x i32)
i32.const 1
i32.const 2
call $file_2.A
set_local $x

file_2.func_2
)
(export "func_1" (func $func_1))
)