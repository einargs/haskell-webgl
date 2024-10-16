import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import "./styles.css";

function customLogger(tag) {
  const decoder = new TextDecoder("utf-8", { fatal: false });
  let str_buf = "";
  return new ConsoleStdout((buffer) => {
    const str = decoder.decode(buffer, {stream:true});
    if (str.trim()) console.log(`[${tag}] ${str}`);
  })
}

function configureWASI() {
  
  let args = ["bin", "arg1", "arg2"];
  let env = ["FOO=bar"];
  let fds = [
      new OpenFile(new File([])), // stdin
      ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
      customLogger("WASI stderr"),
      //ConsoleStdout.lineBuffered(msg => console.log(`[WASI stderr] ${msg}`)),
      /*new PreopenDirectory(".", [
      ]),*/
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
  let req = fetch("/haskell/haskell.wasm")
    .catch(console.error);
  let module = await WebAssembly.instantiateStreaming(req, {
    // @ts-expect-error
    ghc_wasm_jsffi: (await import("../haskell/haskell.ffi.js")).default(__exports),
    "wasi_snapshot_preview1": wasi.wasiImport,
  });

  // @ts-expect-error
  wasi.initialize(module.instance);
  Object.assign(__exports, module.instance.exports);

  __exports.hs_init(0,0);
  __exports.myMain();
  console.log("Success!");
}
run();
/*
if (import.meta.webpackHot) {
  import.meta.webpackHot.decline('/haskell/haskell.wasm');
  console.log(import.meta.webpackHot);
}
*/
