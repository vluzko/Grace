let fs = require("fs");
const async_utils = require("./async_utils").utils;

let async_it = async_utils.get_async_it(describe);
let async_desc = async_utils.get_async_desc(describe);
describe("Simple WASM tests.", function () {

  async_it("Testing.", () => {
      let module_as_bytes = new Uint8Array(fs.readFileSync(
	  "spec/outputs/simple_add.wasm"));
    return WebAssembly.instantiate(module_as_bytes);
  }, module => {
    expect(module.instance.exports.add(2,3)).toBe(5);
  });

  test('table_test.', async (done) => {
    let module_as_bytes = new Uint8Array(fs.readFileSync("spec/outputs/table_test.wasm"));
    let module = await WebAssembly.instantiate(module_as_bytes);
    let first_call = module.instance.exports.callByIndex(0);
    let second_call = module.instance.exports.callByIndex(1);
    expect(first_call).toBe(42);
    expect(second_call).toBe(13);
    done();
  })

});

describe("gradual tests.", () => {
    let mem_module;
    let grad_module;
    let grad_funcs;

    beforeAll(async (done) => {
        mem_module = (await async_utils.compile_wat("../src/builtins/memory_management.wat")).instance.exports;
        const imports = {'memory_management': mem_module};
        grad_module = await async_utils.compile_wat("../src/builtins/gradual_binary_ops.wat", imports);
        grad_funcs = grad_module.instance.exports;
        done()
    })

    test('wrap_i32.', async (done) => {
        const wrapped = grad_funcs.wrap_i32(5);
        const type_val = mem_module.inspect(wrapped);
        const data_val = mem_module.inspect(wrapped + 4);
        expect(type_val).toBe(0);
        expect(data_val).toBe(5);
        done();
    })
    
    test('add_i32.', async (done) => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 0);
        mem_module.set(b_ptr, 0);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res_ptr = grad_funcs.add_i32(a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect(res_ptr + 4);
        expect(type_res).toBe(0);
        expect(data_res).toBe(9);
        done();
    })
    
    test('add_gradual.', async (done) => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 0);
        mem_module.set(b_ptr, 0);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res_ptr = grad_funcs.add_i32(a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect(res_ptr + 4);
        expect(type_res).toBe(0);
        expect(data_res).toBe(9);
        done();
    })

    test('gradual_binary test add f32.', async (done) => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 2);
        mem_module.set(b_ptr, 2);
        mem_module.set_f32(a_ptr+4, 2);
        mem_module.set_f32(b_ptr+4, 7);
        const res_ptr = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect_f32(res_ptr + 4);
        expect(type_res).toBe(2);
        expect(data_res).toBe(9);
        done();
    })

    test('gradual_binary test add f64.', async (done) => {
        const a_ptr = mem_module.alloc_words(3);
        const b_ptr = mem_module.alloc_words(3);
        mem_module.set(a_ptr, 3);
        mem_module.set(b_ptr, 3);
        mem_module.set_f64(a_ptr+4, 2);
        mem_module.set_f64(b_ptr+4, 7);
        const res_ptr = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect_f64(res_ptr + 4);
        expect(type_res).toBe(3);
        expect(data_res).toBe(9);
        done();
    })
    
    test('gradual_binary_test.', async (done) => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 1);
        mem_module.set(b_ptr, 1);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        console.log(res)
        const type_res = mem_module.inspect(res);
        const data_res = mem_module.inspect(res + 4);
        expect(type_res).toBe(1);
        expect(data_res).toBe(9);
        // expect(first_call).toBe(7);
        done();
    })
})

fdescribe("Small grace tests.", () => {
    test('Arithmetic operators', async () => {
        const module = await async_utils.compile_grace("spec/inputs/small_grace.gr", "spec/outputs/small_grace.wat");
        expect(module.instance.exports.add(2, 3)).toBe(5);
        expect(module.instance.exports.sub(2, 3)).toBe(-1);
        expect(module.instance.exports.mult(2, 3)).toBe(6);
        expect(module.instance.exports.div(2, 3)).toBe(2/3);
    })

//   async_desc("", () => {
//     return async_utils.compile_grace("spec/inputs/small_grace.gr",
//       "spec/outputs/small_grace.wat");
//   }, [[
//     'arithmetic operators', module => {
//     expect(module.instance.exports.add(2, 3)).toBe(5);
//     expect(module.instance.exports.sub(2, 3)).toBe(-1);
//     expect(module.instance.exports.mult(2, 3)).toBe(6);
//     expect(module.instance.exports.div(2, 3)).toBe(2/3);
//   }], [
//     'control flow', module => {
//     expect(module.instance.exports.conditional(2,3)).toBe(3);
//     expect(module.instance.exports.loop(2, 1)).toBe(1);
//     expect(module.instance.exports.loop(3, -2)).toBe(-2);
//     expect(module.instance.exports.loop(0)).toBe(0);
//   }], [
//     'comparison operators', module => {
//     expect(module.instance.exports.equality(0, 1)).toBe(0);
//     expect(module.instance.exports.equality(0, 0)).toBe(1);

//     expect(module.instance.exports.neq(0, 0)).toBe(0);
//     expect(module.instance.exports.neq(0, 1)).toBe(1);

//     expect(module.instance.exports.less(0, 0)).toBe(0);
//     expect(module.instance.exports.less(0, 1)).toBe(1);
//     expect(module.instance.exports.less(1, 0)).toBe(0);

//     expect(module.instance.exports.lesse(0, 0)).toBe(1);
//     expect(module.instance.exports.lesse(0, 1)).toBe(1);
//     expect(module.instance.exports.lesse(1, 0)).toBe(0);
//   }], [
//     'function calls', module => {
//     expect(module.instance.exports.call_func(2, 3)).toBe(5);
//   }], [
//     "logical operators", module => {
//     expect(module.instance.exports.and_test(1, 1)).toBe(1);
//     expect(module.instance.exports.or_test(1, 1)).toBe(1);
//     expect(module.instance.exports.xor_test(1, 0)).toBe(1);
//   }]]);
});

describe("Memory management tests.", function() {
  let mem_manage;
  afterEach(function () {
    mem_manage.obliviate();
  });
  async_desc("", () => {
    return async_utils.compile_wat("../src/builtins/memory_management.wat").then(module => {
      mem_manage = module.instance.exports;
      return module.instance.exports;
    });
  }, [[
    'memory tests', module => {
    expect(module.alloc(4)).toBe(12);
    expect(module.alloc(40)).toBe(24);
    expect(module.alloc(40)).toBe(72);
    expect(module.inspect(4)).toBe(16);
    expect(module.inspect(16)).toBe(64);
    expect(module.inspect(64)).toBe(0);
    expect(module.free_chunk(24)).toBe(1);
    expect(module.inspect(4)).toBe(64);
    expect(module.alloc(40)).toBe(24);
  }], [
    "copy_many", module => {
      expect(module.alloc(12)).toBe(12);
      module.set(12, 20);
      module.set(16, 30);
      module.set(20, 40);
      module.copy_many(12, 30, 12);
      expect(module.inspect(30)).toBe(20);
      expect(module.inspect(34)).toBe(30);
      expect(module.inspect(38)).toBe(40);
    }
  ]]);
});

describe("Array tests.", function () {
  let mem_manage;

  afterEach(function () {
    mem_manage.obliviate();
  });

  async_desc("", () => {
    return async_utils.compile_wat("../src/builtins/memory_management.wat").then(module => {
      let imports = {
        "memory_management": module.instance.exports
      };
      mem_manage = module.instance.exports;
      return async_utils.compile_wat("../src/builtins/arrays.wat", imports).then(mod => mod.instance.exports);
    });

  }, [[
    "create arrays", module=> {
    expect(module.create_array(10, 4)).toBe(12);
  }], [
    "set and get elements.", module => {
    let array = module.create_array(10, 4);
    expect(array).toBe(12);
    // Set the third element of the array to 2.
    module.set_value(-2, array, 3, 4);
    expect(mem_manage.inspect(24)).toBe(-2);
    expect(module.get_value(array, 3, 4)).toBe(-2);

  }], [
    "delete array.", module => {
      module.create_array(5, 1);
      let second = module.create_array(5, 1);
      module.create_array(5, 1);
      let resized = module.delete(second);
      expect(mem_manage.inspect(0)).toBe(4);
      expect(mem_manage.inspect(4)).toBe(60);
    }
  ], [
    "resize only array.", module => {
      let first = module.create_array(10, 4);
      let second = module.resize(first, 10, 4);
      expect(second).toBe(12);
    }
  ], [
    "resize array in middle", module => {
      module.create_array(5, 1);
      let second = module.create_array(5, 1);
      module.create_array(5, 1);
      let resized = module.resize(second, 10, 1);
      expect(mem_manage.inspect(0)).toBe(4);
      expect(mem_manage.inspect(4)).toBe(60);
      expect(mem_manage.inspect(60)).toBe(88);
      expect(resized).toBe(96); // pointer to data segment of chunk that starts at 88
    }
  ]])
});

// before resize, chunks start at 4, 32, and 60
// after resize, chunks start at 4, 60, and 88
