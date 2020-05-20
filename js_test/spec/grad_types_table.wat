(module
    (import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
    (import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
    (import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
    (import "memory_management" "mem" (memory (;0;) 1))

    (table $tb 2 anyfunc)
    (func $f1 (result i32)
        i32.const 42)
    (func $f2 (result i32)
        i32.const 13)
    ;; (elem (i32.const 0) $f1 $f2)
    (type $return_i32 (func (result i32)))

    (func $unwrap_i32 (param $ptr i32) (result i32)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        ;; Get the raw data
        i32.load
    )

    (func $unwrap_i64 (param $ptr i32) (result i64)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        ;; Get the raw data
        i64.load
    )

    (func $unwrap_f32 (param $ptr i32) (result f32)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        ;; Get the raw data
        f32.load
    )

    (func $unwrap_f64 (param $ptr i32) (result f64)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        ;; Get the raw data
        f64.load
    )

    (func $wrap_i32 (param $val i32) (local $ptr i32) (result i32)
        ;; Allocate data for the gradual object
        i32.const 2
        call $.memory_management.alloc_words
        tee_local $ptr

        ;; Store the type
        i32.const 0
        i32.store

        ;; Store the data
        get_local $ptr
        i32.const 4
        i32.add
        get_local $val
        i32.store

        ;; Return the pointer.
        get_local $ptr
    )


    (func $add_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.add

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $add_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.add

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $add_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.add

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $add_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.add

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $sub_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.sub

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $sub_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.sub

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $sub_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.sub

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $sub_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.sub

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $mul_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.mul

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $mul_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.mul

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $mul_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.mul

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $mul_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.mul

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $div_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.div

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $div_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.div

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $div_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.div

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $div_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.div

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $eq_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.eq

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $eq_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.eq

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $eq_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.eq

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $eq_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.eq

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $ne_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.ne

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $ne_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.ne

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $ne_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.ne

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $ne_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.ne

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $lt_s_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.lt_s

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $lt_s_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.lt_s

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $lt_s_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.lt_s

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $lt_s_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.lt_s

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $le_s_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.le_s

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $le_s_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.le_s

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $le_s_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.le_s

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $le_s_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.le_s

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $gt_s_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.gt_s

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $gt_s_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.gt_s

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $gt_s_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.gt_s

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $gt_s_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.gt_s

        ;; Rewrap and return.
        call $wrap_f64
    )


    (func $ge_s_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Add them
        i32.ge_s

        ;; Rewrap and return.
        call $wrap_i32
    )


    (func $ge_s_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Add them
        i64.ge_s

        ;; Rewrap and return.
        call $wrap_i64
    )


    (func $ge_s_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Add them
        f32.ge_s

        ;; Rewrap and return.
        call $wrap_f32
    )


    (func $ge_s_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Add them
        f64.ge_s

        ;; Rewrap and return.
        call $wrap_f64
    )

    ;; Return a pointer to the addition function to use for the given type.
    ;; The pointer is the index of the function in the *table*.
    (func $choose_add (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 0
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 1
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 2
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 3
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )
    (type $generic_binary (func (param $a i32) (param $b i32) (result i32)))
    (func $add_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct addition function.
        call $choose_add

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
    )

    (func $choose_sub (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 4
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 5
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 6
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 7
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_mul (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 8
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 9
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 10
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 11
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_div (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 12
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 13
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 14
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 15
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_eq (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 16
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 17
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 18
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 19
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_ne (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 20
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 21
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 22
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 23
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_lt_s (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 24
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 25
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 26
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 27
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_le_s (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 28
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 29
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 30
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 31
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_gt_s (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 33
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 34
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 35
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )


    (func $choose_ge_s (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const 36
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const 37
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const 38
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const 39
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
        )
    )

    (elem (i32.const 0) $add_i32 $add_i64 $add_f32 $add_f64)
    (func (export "callByIndex") (param $i i32) (result i32)
        local.get $i
        call_indirect (type $return_i32))
    (export "tb" (table $tb))
)