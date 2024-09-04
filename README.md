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

## Run
Use `./compile.sh` to compile the haskell code, and `npm run serve`
to run a hot module reloading system, and `npm run build` to build it for
deployment somewhere.
