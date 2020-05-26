(module
    (import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
    (import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
    (import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
    (import "memory_management" "mem" (memory (;0;) 1))

    (func $unwrap_i32 (param $ptr i32) (result i32)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        i32.add
        ;; Get the raw data
        i32.load
    )
 
    (func $unwrap_i64 (param $ptr i32) (result i64)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        i32.add
        ;; Get the raw data
        i64.load
    )
 
    (func $unwrap_f32 (param $ptr i32) (result f32)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        i32.add
        ;; Get the raw data
        f32.load
    )
 
    (func $unwrap_f64 (param $ptr i32) (result f64)
        ;; Shift the pointer to point to the data.
        get_local $ptr
        i32.const 4
        i32.add
        ;; Get the raw data
        f64.load
    )
 
    (func $wrap_i32 (param $val i32) (result i32) (local $ptr i32)
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
 
    (func $wrap_i64 (param $val i64) (result i32) (local $ptr i32)
        ;; Allocate data for the gradual object
        i32.const 3
        call $.memory_management.alloc_words
        tee_local $ptr
 
        ;; Store the type
        i32.const 1
        i32.store
 
        ;; Store the data
        get_local $ptr
        i32.const 4
        i32.add
        get_local $val
        i64.store
 
        ;; Return the pointer.
        get_local $ptr
    )
 
    (func $wrap_f32 (param $val f32) (result i32) (local $ptr i32)
        ;; Allocate data for the gradual object
        i32.const 2
        call $.memory_management.alloc_words
        tee_local $ptr
 
        ;; Store the type
        i32.const 2
        i32.store
 
        ;; Store the data
        get_local $ptr
        i32.const 4
        i32.add
        get_local $val
        f32.store
 
        ;; Return the pointer.
        get_local $ptr
    )
 
    (func $wrap_f64 (param $val f64) (result i32) (local $ptr i32)
        ;; Allocate data for the gradual object
        i32.const 3
        call $.memory_management.alloc_words
        tee_local $ptr
 
        ;; Store the type
        i32.const 3
        i32.store
 
        ;; Store the data
        get_local $ptr
        i32.const 4
        i32.add
        get_local $val
        f64.store
 
        ;; Return the pointer.
        get_local $ptr
    )


    (table $tb 2 anyfunc)
    (func $f1 (result i32)
        i32.const 42)
    (func $f2 (result i32)
        i32.const 13)
    ;; (elem (i32.const 0) $f1 $f2)
    (type $return_i32 (func (result i32)))
    (type $generic_binary (func (param $a i32) (param $b i32) (result i32)))

        
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

   (func $add_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_add

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
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

   (func $sub_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_sub

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
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

   (func $mul_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_mul

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
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
        call $wrap_i32
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
        call $wrap_i32
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
        call $wrap_i32
    )


    (func $choose_eq (param $type_id i32) (result i32)
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

   (func $eq_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_eq

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
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
        call $wrap_i32
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
        call $wrap_i32
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
        call $wrap_i32
    )


    (func $choose_ne (param $type_id i32) (result i32)
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

   (func $ne_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_ne

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
    )

    (elem (i32.const 0) $add_gradual $sub_gradual $mul_gradual $eq_gradual $ne_gradual)
    (func (export "callByIndex") (param $i i32) (param $a i32) (param $b i32) (result i32)
        get_local $i
        get_local $a
        get_local $b
        call_indirect (type $generic_binary))
)