(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "tee_memory" (func $.memory_management.tee_memory (param $loc i32) (param $value i32) (result i32)))
(import "gradual_binary_ops" "call_gradual" (func $.gradual_binary_ops.call_gradual (param $i i32) (param $a i32) (param $b i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $add (param $a i32) (param $b i32) (result i32) (local $x i32)
get_local $a
get_local $b
i32.add
set_local $x
get_local $x
)
(export "add" (func $add))

(func $sub (param $a i32) (param $b i32) (result i32) (local $x i32)
get_local $a
get_local $b
i32.sub
set_local $x
get_local $x
)
(export "sub" (func $sub))

(func $mult (param $a i32) (param $b i32) (result i32) (local $x i32)
get_local $a
get_local $b
i32.mul
set_local $x
get_local $x
)
(export "mult" (func $mult))

(func $div (param $a f64) (param $b f64) (result f64) (local $x f64)
get_local $a
get_local $b
f64.div
set_local $x
get_local $x
)
(export "div" (func $div))

(func $equality (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.eq
)
(export "equality" (func $equality))

(func $neq (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.ne
)
(export "neq" (func $neq))

(func $less (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.lt_s
)
(export "less" (func $less))

(func $lesseq (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.le_s
)
(export "lesseq" (func $lesseq))

(func $great (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.gt_s
)
(export "great" (func $great))

(func $greateq (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.ge_s
)
(export "greateq" (func $greateq))

(func $and_test (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.and
)
(export "and_test" (func $and_test))

(func $or_test (param $a i32) (param $b i32) (result i32)
get_local $a
get_local $b
i32.or
)
(export "or_test" (func $or_test))
)
