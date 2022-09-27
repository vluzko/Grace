(module
    (func $convert_test_pass (result i32)
        i32.const 0
        f32.convert_s/i32
        drop
        i32.const 0
        f32.convert_u/i32
        drop

        i32.const 0
        f64.convert_s/i32
        drop
        i32.const 0
        f64.convert_u/i32
        drop

        i32.const 0

    )
    (export "convert_test_pass" (func $convert_test_pass))

    (func $convert_test_pass_2 (result i32)
        i32.const 0
        f32.convert_u/i32
        drop

        i32.const 0

    )
    (export "convert_test_pass_2" (func $convert_test_pass_2))

    (func $convert_test_fail_1 (result i32)
        i32.const 0
        f32.convert_i32_s
        drop

        ;; i32.const 0
        ;; f32.convert_i32_u
        ;; drop

        ;; i32.const 0
        ;; f64.convert_i32_s
        ;; drop
        ;; i32.const 0
        ;; f64.convert_i32_u
        ;; drop

        ;; f32.const 0
        ;; f64.promote_f32
        ;; drop

        i32.const 0

    )
    (export "convert_test_fail_1" (func $convert_test_fail_1))

    (func $convert_test_fail_2 (result i32)
        i32.const 0
        f32.convert_i32_u
        drop

        i32.const 0
    )
    (export "convert_test_fail_2" (func $convert_test_fail_2))

    (func $convert_test_fail_3 (result i32)
        i32.const 0
        f64.convert_i32_s
        drop

        i32.const 0
    )
    (export "convert_test_fail_3" (func $convert_test_fail_3))

    (func $convert_test_fail_4 (result i32)
        i32.const 0
        f64.convert_i32_u
        drop

        i32.const 0
    )
    (export "convert_test_fail_4" (func $convert_test_fail_4))

    (func $convert_test_fail_5 (result i32)
        f32.const 0
        f64.promote_f32
        drop

        i32.const 0 
    )
    (export "convert_test_fail_5" (func $convert_test_fail_5))

)
