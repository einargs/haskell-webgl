const path = require('path');
const Watchpack = require('watchpack');
const { spawn } = require('node:child_process');
const fs = require('node:fs/promises');
const { postLink } = import(process.env["POST_LINK"]);

// Taken from the POST_LINK file that GHC provides.
async function produceFFI(wasmFileSource) {
  console.log(wasmFileSource.constructor);
  return await postLink(await WebAssembly.compile());
}

// The problem is that it does the recompile but doesn't
// reload Main.wasm.
//
// - it detects the new stuff at the start of webpack
// - it detects the change and does the rebuild
// - but the HMR doesn't realize there's a change.
module.exports = class HaskellPlugin {
  constructor({cabalFile, haskellDir, debugMode, wasmPath}) {
    this.cabalFile = cabalFile;
    this.haskellDir = haskellDir;
    this.cabalPath = process.env["WASM_CABAL"];
    this.hasSetup = false;
    this.debugMode = debugMode;
    this.compiling = Promise.resolve();
    this.wasmPath = wasmPath;
    this.wp = new Watchpack({
    });
  }

  apply(compiler) {
    const pluginName = HaskellPlugin.name;
    const logger = compiler.getInfrastructureLogger(pluginName);
    const { RawSource } = compiler.webpack.sources;
    const debug = (...args) => {
      if (this.debugMode) {
        logger.log(...args);
      }
    };
    compiler.hooks.beforeCompile.tapPromise(pluginName, async ({normalModuleFactory}) => {
      normalModuleFactory.hooks.module.tap(pluginName, (module, createData, resolveData) => {
        console.log(module, createData, resolveData);
      });
      debug("Before hook called.");
      if (this.hasSetup) {
        return this.compiling;
      }
      this.hasSetup = true;
      this.wp.watch({
        files: [
          path.resolve(__dirname, this.cabalFile)
        ],
        directories: [
          path.resolve(__dirname, this.haskellDir)
        ],
        // files assumed to not exist, so no remove
        // event is fired when they aren't there on
        // start.
        missing: [],
        startTime: Date.now() - 10000
      });
      debug("Setting up, beginning initial compile.")
      this.compiling = this._buildHaskellCode(debug);
      let first = true;
      this.wp.on("aggregated", (changes, removals) => {
        debug("Aggregated handler called.", changes, removals);
        if (first) {
          first = false;
        } else {
          debug("Compiling again.");
          this.compiling = this._buildHaskellCode(debug);
        }
      });
      return this.compiling;
    });
    compiler.hooks.compilation.tap(pluginName, (compilation) => {
      compilation.hooks.processAssets.tapPromise(pluginName, async () => {
        /*
        let [ffiContents, wasmContents] = await Promise.all([
          fs.readFile(path.join(__dirname, "./hs-artifacts/Main_ffi.js")),
          fs.readFile(path.join(__dirname, "./hs-artifacts/Main.wasm"))
        ]);*/
        let wasmContents = await fs.readFile(this.wasmPath);
        let ffiContents = await produceFFI(wasmContents);
        compilation.emitAsset(
          "Main.wasm",
          new RawSource(wasmContents)
        );
        compilation.emitAsset(
          "Main.wasm",
          new RawSource(wasmContents)
        );
      });
    });
  }

  async _callCabalBuild(debug) {
    return new Promise((resolve, reject) => {
      const p = spawn(this.cabalPath, ["build"], {
        cwd: __dirname,
        stdio: 'inherit'
      })

      p.on('close', (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new Error('Haskell compilation.'));
        }
      });

      p.on('error', reject);
    });
  }

  async _buildHaskellCode(debug) {
    try {
      await this._callCabalBuild(debug);
      debug("compilation success.");
    } catch (err) {
      debug("Compilation error", err);
      throw err;
    }
  }
}
