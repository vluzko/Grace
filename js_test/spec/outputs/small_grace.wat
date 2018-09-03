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
)
