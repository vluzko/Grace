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
    'binary operators', module => {
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
  }]]);
});

describe("Wat tests.", function() {
  async_desc("", () => {
    return async_utils.compile_wat("js_test/spec/outputs/wat_test.wat");
  }, [[
    'memory tests', module => {
    console.log("Memory size is", module.instance.exports.array());
    console.log(module.instance.exports.loadstuff());
    }]]);
});