(module
(func $conditional (param $a i32) (param $b i32) (result i32) 
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
(func $div (param $a i32) (param $b i32) (result i32) (local $x i32)
get_local $a
get_local $b
i32.div_s
set_local $x
get_local $x
)
(export "div" (func $div))
(func $loop  (result i32) (local $x i32)
i32.const 2
set_local $x
block $void1
 loop $void (result i32)
get_local $x
br_if 1

get_local $x
i32.const 1
i32.sub
set_local $x
br 0
end
end

get_local $x
)
(export "loop" (func $loop))
)
