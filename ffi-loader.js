import path from 'path';

export default function (source) {
  console.log("ffi source", source.slice(0,50));
  let callback = this.async();
  let wasmPath = path.resolve('./haskell.wasm');
  console.log("wasm path", wasmPath);
  this.addDependency(wasmPath);
  callback(null, source);
  return;
}
