#!/usr/bin/env bash
# We call cabal to build
wasm32-wasi-cabal build
# this is where the wasm output is
WASM_PATH="dist-newstyle/build/wasm32-wasi/ghc-9.11.20240828/haskell-webgl-0.1.0.0/x/haskell-webgl/opt/build/haskell-webgl/haskell-webgl.wasm"
# Copy to the haskell artifacts
cp $WASM_PATH ./hs-artifacts/Main.wasm
# Produce the corresponding JS ffi
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i hs-artifacts/Main.wasm -o hs-artifacts/Main_ffi.js
