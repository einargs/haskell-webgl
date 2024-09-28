# Haskell in Web Assembly
This is Haskell running in web assembly. The idea is to use this to play around
with webgl some.

## Resources
See:

- https://gitlab.haskell.org/ghc/ghc-wasm-meta for resources on how to get
  started
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api
  for information on how to use the JS FFI system, since there are additional
  initialization and compilation steps.
- https://finley.dev/blog/2024-08-24-ghc-wasm.html
  for a very useful blog post about everything.

## Setting up
Use the nix package manager to install all the dependencies needed; just run
`nix develop` once flakes are enabled.

## Bugs
- Currently for some reason the live reloading on the second change after the
  initial compile. Who knows why.

## Hot Module Reloading with Haskell
To setup fully functioning hot module reloading I would need
to setup a way to tear down the wasm reactor and replace it. That should
actually be quite possible, but it'd be more dev time and I'm not sure it'd be
worth it? Since the haskell code itself isn't split up to make it useful. I'd
have to investigate exactly how powerful HMR is.

Instead at the end I guess I'm just going to force a reload every time.

### wasm-hmr-loader.ts
This was an experiment in keeping HMR for everything except
the haskell code, and forcing a full reload for that.
It's run into the problem that if I register a loader for
haskell/haskell.wasm, the contents then conflict. But the
hot module reload tech throws an error if I don't have
a loader for it. And loaders are supposed to return javascript.

I suppose it makes sense that for HMR you would want to follow
webpack's style of packaging everything as javascript, and then
for production use a different method.
```typescript
import { Buffer } from 'node:buffer';
import { RawLoaderDefinition } from 'webpack';
import path from 'path';

// takes a buffer
const wasmHMRLoader: RawLoaderDefinition = function(source: Buffer) {
  console.log("wasmHMRLoader.resourcePath", this.resourcePath);
  this.callback(null, `
if (import.meta.webpackHot) {
  console.info("Can't yet do hot module reloading for haskell code");
  import.meta.webpackHot.decline();
}
  `);
}
wasmHMRLoader.raw = true;
export default wasmHMRLoader;
```

The [HMR API Docs](https://webpack.js.org/api/hot-module-replacement/)
have some more information. There are tools to e.g. let you register
a callback for when things are replaced, etc. If I implemented a way
to split up haskell components, I could probably do that pretty easily.

# Future Name
`haskell-web` is definitely just a temporary name. Maybe `Aschetil`,
a root name for Haskell?
