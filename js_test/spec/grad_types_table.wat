(module
    (table $tb 2 anyfunc)
    (func $f1 (result i32)
        i32.const 42)
    (func $f2 (result i32)
        i32.const 13)
    ;; (elem (i32.const 0) $f1 $f2)
    (type $return_i32 (func (result i32)))
    (func $add_i32 (param $a i32) (param $b i32) (result i32)
        local.get $a
        local.get $b
        i32.add
    )

    (func $add_i64 (param $a i32) (param $b i32) (result i32)
        local.get $a
        local.get $b
        i64.add
    )

    (func $add_f32 (param $a i32) (param $b i32) (result i32)
        ;; unwrap a
        ;; unwrap b
        ;; add the results
        ;; wrap the result
        ;; return
        local.get $a
        local.get $b
        f32.add
    )

    (func $add_f64 (param $a i32) (param $b i32) (result i32)
        local.get $a
        local.get $b
        f64.add
    )

    (func $choose_add (param $type_id i32) (result i32)
    )

    (elem (i32.const 0) $add_i32 $add_i64 $add_f32 $add_f64)
    (func (export "callByIndex") (param $i i32) (result i32)
        local.get $i
        call_indirect (type $return_i32))
    (export "tb" (table $tb))
)