let fs = require("fs");
const async_utils = require("./async_utils").utils;

let async_it = async_utils.get_async_it(describe);

describe("Simple WASM test.", function () {

  async_it("Testing.", () => {
    let module_as_bytes = new Uint8Array(fs.readFileSync("spec/outputs/simple_add.wasm"));
    return WebAssembly.instantiate(module_as_bytes);
  }, module => {
    expect(module.instance.exports.add(2,3)).toBe(5);
  });

});

describe("Full tests", function () {
  async_it("test.", () => {
    return async_utils.compile_grace("js_test/spec/inputs/small_grace.gr", "js_test/spec/outputs/small_grace.wat");
  }, module => {
    console.log(module);
  });
});
