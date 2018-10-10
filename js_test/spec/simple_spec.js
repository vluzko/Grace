let fs = require("fs");
const async_utils = require("./async_utils").utils;

let async_it = async_utils.get_async_it(describe);
let async_desc = async_utils.get_async_desc(describe);
describe("Simple WASM test.", function () {

  async_it("Testing.", () => {
      let module_as_bytes = new Uint8Array(fs.readFileSync(
	  "spec/outputs/simple_add.wasm"));
    return WebAssembly.instantiate(module_as_bytes);
  }, module => {
    expect(module.instance.exports.add(2,3)).toBe(5);
  });

});

describe("Small grace tests.", function () {
  async_desc("", () => {
    return async_utils.compile_grace("js_test/spec/inputs/small_grace.gr",
      "js_test/spec/outputs/small_grace.wat");
  }, [[
    'arithmetic operators', module => {
    expect(module.instance.exports.add(2, 3)).toBe(5);
    expect(module.instance.exports.sub(2, 3)).toBe(-1);
    expect(module.instance.exports.mult(2, 3)).toBe(6);
    expect(module.instance.exports.div(2, 3)).toBe(0);
  }], [
    'control flow', module => {
    expect(module.instance.exports.conditional(2,3)).toBe(3);
    expect(module.instance.exports.loop(2, 1)).toBe(1);
    expect(module.instance.exports.loop(3, -2)).toBe(-2);
    expect(module.instance.exports.loop(0)).toBe(0);
  }], [
    'comparison operators', module => {
    expect(module.instance.exports.equality(0, 1)).toBe(0);
    expect(module.instance.exports.equality(0, 0)).toBe(1);

    expect(module.instance.exports.neq(0, 0)).toBe(0);
    expect(module.instance.exports.neq(0, 1)).toBe(1);

    expect(module.instance.exports.less(0, 0)).toBe(0);
    expect(module.instance.exports.less(0, 1)).toBe(1);
    expect(module.instance.exports.less(1, 0)).toBe(0);

    expect(module.instance.exports.lesse(0, 0)).toBe(1);
    expect(module.instance.exports.lesse(0, 1)).toBe(1);
    expect(module.instance.exports.lesse(1, 0)).toBe(0);
  }], [
    'function calls', module => {
    expect(module.instance.exports.call_func(2, 3)).toBe(5);
  }], [
    "logical operators", module => {
    expect(module.instance.exports.and_test(1, 1)).toBe(1);
    expect(module.instance.exports.or_test(1, 1)).toBe(1);
    expect(module.instance.exports.xor_test(1, 0)).toBe(1);
  }]]);
});

describe("Wat tests.", function() {
  async_desc("", () => {
    return async_utils.compile_wat("js_test/spec/outputs/memory_management.wat");
  }, [[
    'memory tests', module => {
    expect(module.instance.exports.alloc(1)).toBe(12);
    expect(module.instance.exports.alloc(10)).toBe(24);
    expect(module.instance.exports.alloc(10)).toBe(72);
    expect(module.instance.exports.inspect(4)).toBe(16);
    expect(module.instance.exports.inspect(16)).toBe(64);
    expect(module.instance.exports.inspect(64)).toBe(0);
    expect(module.instance.exports.free_chunk(24)).toBe(1);
    expect(module.instance.exports.inspect(4)).toBe(64);
    expect(module.instance.exports.alloc(10)).toBe(24);
  }]]);
});

describe("Array tests.", function () {
  let mem_manage;
  async_desc("", () => {
    return async_utils.compile_wat("js_test/spec/outputs/memory_management.wat").then(module => {
      let imports = {
        "memory_management": module.instance.exports
      };
      mem_manage = module.instance.exports;
      return async_utils.compile_wat("js_test/spec/outputs/arrays.wat", imports);
    });

  }, [[
    "create arrays", module=> {
    expect(module.instance.exports.create_array(10, 10)).toBe(12);
    // console.log(module.instance.exports.create_array(0 , 0));
  }], [
    "set and get elements.", module => {
    let array = module.instance.exports.create_array(10, 10);
    expect(array).toBe(12);
    // Set the third element of the array to 2.
    module.instance.exports.set_value(-2, array, 3, 4);
    expect(mem_manage.inspect(24)).toBe(-2);
    expect(module.instance.exports.get_value(array, 3, 4)).toBe(-2);

  }]])
});

