# This cannot be run in wasmtime because we don't generate main.
wasm32-wasi-ghc src/Main.hs -o hs-artifacts/Main.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init,--export=myMain
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i hs-artifacts/Main.wasm -o hs-artifacts/Main_ffi.js
