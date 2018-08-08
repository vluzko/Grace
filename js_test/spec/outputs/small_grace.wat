(module
(func $a (param $b i64) (result i64)
i64.const 5
i64.const 6
i64.add
set_local $x
get_local $x
)
(export "a" (func $a))
)
