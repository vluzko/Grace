(module
    (import "memory_management" "alloc_words" (func $.memory_management.alloc_words (param $a i32) (result i32)))
    ;; (import "memory_management" "free_chunk" (func $.memory_management.free_chunk (param $a i32) (result i32)))
    ;; (import "memory_management" "copy_many" (func $.memory_management.copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
    (import "memory_management" "mem" (memory (;0;) 1))

    (table $tb 5 anyfunc)
    (type $generic_binary (func (param $a i32) (param $b i32) (result i32)))
(type $return_i32 (func (result i32)))
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
    ) (export "wrap_i32" (func $wrap_i32))
 
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
        
    
    (func $add_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Do the operation
        i32.add

        ;; Rewrap and return.
        call $wrap_i32
    ) (export "add_i32" (func $add_i32))


    (func $add_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Do the operation
        i64.add

        ;; Rewrap and return.
        call $wrap_i64
    ) (export "add_i64" (func $add_i64))


    (func $add_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Do the operation
        f32.add

        ;; Rewrap and return.
        call $wrap_f32
    ) (export "add_f32" (func $add_f32))


    (func $add_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Do the operation
        f64.add

        ;; Rewrap and return.
        call $wrap_f64
    ) (export "add_f64" (func $add_f64))

    (func $add_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call $add_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call $add_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call $add_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call $add_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "add_gradual" (func $add_gradual))

    (func $sub_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Do the operation
        i32.sub

        ;; Rewrap and return.
        call $wrap_i32
    ) (export "sub_i32" (func $sub_i32))


    (func $sub_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Do the operation
        i64.sub

        ;; Rewrap and return.
        call $wrap_i64
    ) (export "sub_i64" (func $sub_i64))


    (func $sub_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Do the operation
        f32.sub

        ;; Rewrap and return.
        call $wrap_f32
    ) (export "sub_f32" (func $sub_f32))


    (func $sub_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Do the operation
        f64.sub

        ;; Rewrap and return.
        call $wrap_f64
    ) (export "sub_f64" (func $sub_f64))

    (func $sub_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call $sub_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call $sub_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call $sub_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call $sub_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "sub_gradual" (func $sub_gradual))

    (func $mul_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Do the operation
        i32.mul

        ;; Rewrap and return.
        call $wrap_i32
    ) (export "mul_i32" (func $mul_i32))


    (func $mul_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Do the operation
        i64.mul

        ;; Rewrap and return.
        call $wrap_i64
    ) (export "mul_i64" (func $mul_i64))


    (func $mul_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Do the operation
        f32.mul

        ;; Rewrap and return.
        call $wrap_f32
    ) (export "mul_f32" (func $mul_f32))


    (func $mul_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Do the operation
        f64.mul

        ;; Rewrap and return.
        call $wrap_f64
    ) (export "mul_f64" (func $mul_f64))

    (func $mul_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call $mul_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call $mul_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call $mul_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call $mul_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "mul_gradual" (func $mul_gradual))

    (func $eq_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Do the comparison and return
        i32.eq
    ) (export "eq_i32" (func $eq_i32))


    (func $eq_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Do the comparison and return
        i64.eq
    ) (export "eq_i64" (func $eq_i64))


    (func $eq_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Do the comparison and return
        f32.eq
    ) (export "eq_f32" (func $eq_f32))


    (func $eq_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Do the comparison and return
        f64.eq
    ) (export "eq_f64" (func $eq_f64))

    (func $eq_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call $eq_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call $eq_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call $eq_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call $eq_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "eq_gradual" (func $eq_gradual))

    (func $ne_i32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i32

        ;; Unwrap b
        get_local $b
        call $unwrap_i32

        ;; Do the comparison and return
        i32.ne
    ) (export "ne_i32" (func $ne_i32))


    (func $ne_i64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_i64

        ;; Unwrap b
        get_local $b
        call $unwrap_i64

        ;; Do the comparison and return
        i64.ne
    ) (export "ne_i64" (func $ne_i64))


    (func $ne_f32 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f32

        ;; Unwrap b
        get_local $b
        call $unwrap_f32

        ;; Do the comparison and return
        f32.ne
    ) (export "ne_f32" (func $ne_f32))


    (func $ne_f64 (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_f64

        ;; Unwrap b
        get_local $b
        call $unwrap_f64

        ;; Do the comparison and return
        f64.ne
    ) (export "ne_f64" (func $ne_f64))

    (func $ne_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call $ne_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call $ne_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call $ne_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call $ne_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "ne_gradual" (func $ne_gradual))


    (elem (i32.const 0) $add_gradual $sub_gradual $mul_gradual $eq_gradual $ne_gradual)
    (func (export "call_gradual") (param $i i32) (param $a i32) (param $b i32) (result i32)
        get_local $a
        get_local $b
        get_local $i

        call_indirect (type $generic_binary)
    )
)