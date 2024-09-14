{
  inputs = {
    #nixpkgs.follows = "wasm-ghc-meta/nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    wasm-ghc-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
    wasm-ghc-meta.inputs.nixpkgs.follows = "nixpkgs";
    #haskell-language-server.url = "github:haskell/haskell-language-server";
  };

  
  /*nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };*/

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    wasm-ghc-meta
  }@inputs: flake-utils.lib.eachSystem ["x86_64-linux"] (system:
  let pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;
      wasm-meta = wasm-ghc-meta.packages.${system};
      wasm-ghc = wasm-meta.wasm32-wasi-ghc-gmp;
      wasm-cabal = wasm-meta.wasm32-wasi-cabal-gmp;
      #wasm-cabal-alias = pkgs.writeShellScriptBin "cabal" "exec -a $0 ${wasm-cabal}/bin/wasm32-wasi-cabal $@";

      # For some reason the default wasm-cabal wrapper isn't working
      # so I'm wrapping it myself.
      custom-wasm-cabal = pkgs.writeShellScriptBin "wasm-cabal" ''
        exec ${wasm-meta.cabal}/bin/cabal \
          --with-compiler=${wasm-ghc}/bin/wasm32-wasi-ghc \
          --with-hc-pkg=${wasm-ghc}/bin/wasm32-wasi-ghc-pkg \
          --with-hsc2hs=${wasm-ghc}/bin/wasm32-wasi-hsc2hs \
          ''${1+"$@"}
      '';
  in {
    # This is because the wasm ghc -- and I think specifically
    # the cabal with custom flags -- is causing problems for the
    # language server.
    #
    # Run with nix develop .#haskell-dev
    devShells.haskell-dev = pkgs.mkShell {
      buildInputs = with pkgs; [
        #haskellPackages.haskell-language-server
        #haskell.compiler.ghcHEAD
        #wasm-meta.cabal
      ];
    };
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        nodejs_22
        # Commenting this out so that I can 
        #wasm-cabal
        #wasm-meta.cabal
        #wasm-cabal-alias
        wasm-meta.default
        custom-wasm-cabal
        haskell.packages.ghc910.haskell-language-server
        #haskell.compiler.ghc910
        haskellPackages.implicit-hie # use with gen-hie
        # this is exposing ghc 9.6 on the PATH for some reason.
        #haskellPackages.haskell-language-server
      ];
      WASM_CABAL = "${wasm-cabal}/bin/wasm32-wasi-cabal";
      shellHook = ''
        export POST_LINK=$(wasm32-wasi-ghc --print-libdir)/post-link.mjs
      '';
    };
  }
  );
}
