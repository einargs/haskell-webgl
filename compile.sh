# This cannot be run in wasmtime because we don't generate main.
wasm32-wasi-ghc src/Main.hs -o public/Main.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init,--export=myMain
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i public/Main.wasm -o public/Main_ffi.js
