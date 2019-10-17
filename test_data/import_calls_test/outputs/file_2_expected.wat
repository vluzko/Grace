(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $A (param $a i32) (param $b i32) (result i32) (local $.x i32)
i32.const 2
call $.memory_management.alloc_words
tee_local $.x
i32.const 8
i32.add
get_local $a
call $.memory_management.set
get_local $.x
i32.const 12
i32.add
get_local $b
call $.memory_management.set
get_local $.x
i32.const 8
i32.add
(export "A" (func $A)

(func $func_2  (result i32) 
i32.const 2
)
(export "func_2" (func $func_2))
)
