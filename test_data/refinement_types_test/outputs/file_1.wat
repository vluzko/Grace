(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $require_ref (param $x i32) (param $y i32) (result i32) 
get_local $x
get_local $y
i32.add
)
(export "require_ref" (func $require_ref))

(func $call_require  (result i32) (local $a.18 i32) (local $b.18 i32)
i32.const 1
set_local $a.18
i32.const 2
set_local $b.18
get_local $a.18
get_local $b.18
call $require_ref
)
(export "call_require" (func $call_require))
)
