{
  inputs = {
    nixpkgs.follows = "wasm-ghc-meta/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    wasm-ghc-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    wasm-ghc-meta
  }@inputs: flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;
      wasm-ghc = wasm-ghc-meta.packages.${system};
  in {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        nodejs_22
        wasm-ghc.default
        #wasm-ghc.wasm32-wasi-ghc-9_10
        # wasm-ghc.wasm32-wasi-cabal-9_10
      ];
    };
  }
  );
}
