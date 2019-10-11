(module
(import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))

(func $conditional (param $a i32) (param $b i32) (result i32) (local $x i32)
get_local $a
get_local $b
i32.add
set_local $x
i32.const 0
if (result i32)
get_local $a
else
get_local $b
end
)
(export "conditional" (func $conditional))
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
(func $div (param $a i32) (param $b i32) (result f64) (local $x f64)
get_local $a
f64.convert_s/i32
get_local $b
f64.convert_s/i32
f64.div
set_local $x
get_local $x
)
(export "div" (func $div))
(func $loop (param $x i32) (param $y i32) (result i32) 
loop $void
block $void1
get_local $x
get_local $y
i32.gt_s
i32.eqz
br_if 0

get_local $x
i32.const 1
i32.sub
set_local $x
br 1
end
end

get_local $x
)
(export "loop" (func $loop))
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
(func $lesse (param $a i32) (param $b i32) (result i32) 
get_local $a
get_local $b
i32.le_s
)
(export "lesse" (func $lesse))
(func $great (param $a i32) (param $b i32) (result i32) 
get_local $a
get_local $b
i32.gt_s
)
(export "great" (func $great))
(func $greate (param $a i32) (param $b i32) (result i32) 
get_local $a
get_local $b
i32.ge_s
)
(export "greate" (func $greate))
(func $call_func (param $a i32) (param $b i32) (result i32) 
get_local $a
get_local $b
call $add
)
(export "call_func" (func $call_func))
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
(func $xor_test (param $a i32) (param $b i32) (result i32) 
get_local $a

get_local $b

i32.xor
)
(export "xor_test" (func $xor_test))
)
