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
 *
 * @param {String} input
 * @param {String} output
 */
function compile_grace(input, output) {
  process.chdir("..");
  // Asynchronously compile the Grace code to WAST.
  let compile_to_wast = exec(`cargo run ${input} ${output}`);
  let wasm_file;
  // Once that's finished, asynchronously compile the WAST to WASM.
  let compile_to_wasm = compile_to_wast.then(({stdout, stderr})=> {
    wasm_file = output.replace(".wat", ".wasm");
    return exec(`wat2wasm ${output} -o ${wasm_file}`);
  });

  // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
  let loaded_module = compile_to_wasm.then(({stdout, stderr}) => {
    let module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
    // Go back to where we started now that we're done running stuff.
    // This doesn't actually fully fix the possible error, but whatever.
    process.chdir("js_test");
    return WebAssembly.instantiate(module_as_bytes);
  });
  return loaded_module;

}
/**
 *
 * @param {String} input
 * @param {String} output
 */
function compile_wat(input, imports) {
  process.chdir("..");
  let wasm_file = input.replace(".wat", ".wasm");
  // Asynchronously compile the WAST to WASM.
  let compile_to_wasm = exec(`wat2wasm ${input} -o ${wasm_file}`);

  // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
  let loaded_module = compile_to_wasm.then(({stdout, stderr}) => {
    let module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
    // Go back to where we started now that we're done running stuff.
    // This doesn't actually fully fix the possible error, but whatever.
    process.chdir("js_test");
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
