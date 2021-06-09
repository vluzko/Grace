"use strict";
let child = require("child_process");
let process = require("process");
let util = require("util");
let fs = require("fs");
let exec = util.promisify(child.exec);

/**
 * Wraps a promise to call done().
 * @param promise
 * @param done
 */
function wrap_promise(promise, done) {
  return promise
    .then(function (data) {
      done();
      return data;
    }, function (err) {
      console.log("Promise error " + err);
      done();
      return err;
    });
}

/**
 * Returns a function which acts as an asynchronous it.
 * Jasmine adds far too much overhead to asynchronous testing, this resolves that.
 * @param describe
 * @return
 */
function get_async_it(describe) {
  return function async_describe(test_name, promiser, expectation) {
    describe(test_name, function () {
      let data;
      beforeEach(function (done) {
        let promise = promiser();
        data = wrap_promise(promise, done);
      });
      it(".it", function (done) {
        data.then(expectation)
          .then(function () {
            done();
          }, function (err) {
            console.log(err);
            done();
          });
      });
    });
  };
}

function get_async_desc(describe) {
  return function async_describe(test_name, promiser, expectations) {
    describe(test_name, function () {
      let data;
      beforeEach(function (done) {
        let promise = promiser();
        data = wrap_promise(promise, done);
      });

      for (let [name, expectation] of expectations) {
        it(name, function (done) {
          data.then(expectation)
            .then(function () {
              done();
            }, function (err) {
              console.log(err);
              done();
            });
        });
      }

    });
  };
}

/**
 * Compile a Grace file to WebAssembly.
 * @param {String} input  - Path to the input file.
 * @param {String} output - Path to the output file.
 */
function compile_grace(input, output) {
    const wasm_file = output.replace(".wat", ".wasm");
    // First clean previous versions.
    try {
        fs.unlinkSync(output);
    } catch (err) {}

    try {
        fs.unlinkSync(wasm_file);
    } catch (err) {}

    // Asynchronously compile the Grace code to WAST.
    let compile_to_wast = exec(`cargo run ${input} ${output} -- --nocapture`);
    // Once that's finished, asynchronously compile the WAST to WASM.
    let compile_to_wasm = compile_to_wast.then(({stdout, stderr})=> {
        console.log(stdout);
        return exec(`wat2wasm ${output} -o ${wasm_file}`);
    });

    // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
    let loaded_module = compile_to_wasm.then(({stdout, stderr}) => {
        const module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
        const memory_management = compile_wat("../src/builtins/memory_management.wat");
        return memory_management.then(mem => {
            const gradual_ops = compile_wat("../src/builtins/gradual_binary_ops.wat");
            return gradual_ops.then(ops => {
                return WebAssembly.instantiate(module_as_bytes, {
                    'memory_management': mem.instance.exports,
                    'gradual_binary_ops': ops.instance.exports
                });
            });
        });
    });
    return loaded_module;

}
/**
 *
 * @param {String} input
 * @param {String} output
 */
function compile_wat(input, imports) {
  let wasm_file = input.replace(".wat", ".wasm");
  // Asynchronously compile the WAST to WASM.
  let compile_to_wasm = exec(`wat2wasm ${input} -o ${wasm_file}`);

  // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
  let loaded_module = compile_to_wasm.then(({stdout, stderr}) => {
    let module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
    return WebAssembly.instantiate(module_as_bytes, imports);
  });
  return loaded_module;
}

exports.utils = {
  get_async_it,
  wrap_promise,
  compile_grace,
  compile_wat,
  get_async_desc
};
