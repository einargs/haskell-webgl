import { Buffer } from 'node:buffer';
import { LoaderDefinition } from 'webpack';
import path from 'path';

// we aren't marking this as raw so it doesn't take a buffer
const ffiLoader: LoaderDefinition = function(source) {
  console.log("resourcePath", this.resourcePath);
  // how do you get the options again?
  let wasmPath = path.resolve('./haskell/haskell.wasm');
  console.log("wasm path", wasmPath);
  this.addDependency(wasmPath);
  this.callback(null, source);
}
export default ffiLoader;
