(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "gradual_binary_ops" "call_gradual" (func $.gradual_binary_ops.call_gradual (param $i i32) (param $a i32) (param $b i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $foo (param $a i64) (result i64) 
get_local $a
)
(export "foo" (func $foo))

(func $bar   (local $b f32)
f32.const 6.0
set_local $b
)
(export "bar" (func $bar))
)
