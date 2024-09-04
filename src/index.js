import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from "@bjorn3/browser_wasi_shim";

function configureWASI() {
  
  let args = ["bin", "arg1", "arg2"];
  let env = ["FOO=bar"];
  let fds = [
      new OpenFile(new File([])), // stdin
      ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
      ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
      new PreopenDirectory(".", [
          ["example.c", new File(new TextEncoder("utf-8").encode(`#include "a"`))],
          ["hello.rs", new File(new TextEncoder("utf-8").encode(`fn main() { println!("Hello World!"); }`))],
      ]),
  ];
  return new WASI(args, env, fds);
}

async function run() {
  let wasi = configureWASI();

  // See this: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api
  // for why we need to do this weird knot tying. Basically we generate a
  // javascript file that implements the FFI calls, and then need to load that
  // and give it the exports from the GHC module.
  let __exports = {};
  let module = await WebAssembly.instantiateStreaming(fetch("./Main.wasm"), {
    ghc_wasm_jsffi: (await import("./Main_ffi.js")).default(__exports),
    "wasi_snapshot_preview1": wasi.wasiImport,
  });

  wasi.initialize(module.instance);
  Object.assign(__exports, module.instance.exports);

  __exports.hs_init(0,0);
  __exports.myMain();
  console.log("Success!");
}
run();
