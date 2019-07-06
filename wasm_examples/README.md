# README

Run the following:

    emcc test.c
    wasm2wat a.out.wasm > test.wat

Probably you will need to activate your emsdk first. Go to the emsdk source folder and run

    source emsdk_env.sh

This will get you the WAST version of your C code. It'll be super gross.