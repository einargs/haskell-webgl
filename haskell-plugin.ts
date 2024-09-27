import * as path from 'path';
import Watchpack from 'watchpack';
import { spawn } from 'node:child_process';
import * as fs from 'node:fs/promises';
import { Buffer } from 'node:buffer';
import webpack, { Compiler, Compilation, ResolveData } from 'webpack';
const { NormalModule } = webpack;
const { postLink }= await import(process.env["POST_LINK"] as string);
import VirtualModulesPlugin from 'webpack-virtual-modules';

import { buildFFILoader } from './ffi-loader.ts';

type CompilationParams = Compilation['params'];
type NormalModuleFactory = CompilationParams['normalModuleFactory'];
type ModuleFactoryCreateData = Parameters<NormalModuleFactory['create']>[0];
type ModuleFactoryResult = NonNullable<Parameters<Parameters<NormalModuleFactory['create']>[1]>[1]>;
type Module = Parameters<typeof webpack.util.comparators.compareModulesByIdentifier>[0];
type ResolveCreateData = ResolveData['createData'];

// Taken from the POST_LINK file that GHC provides.
async function produceFFI(wasmFileSource: Buffer) {
  return await postLink(await WebAssembly.compile(wasmFileSource));
}

function createModule(
  normalModuleFactory: NormalModuleFactory,
  createData: ModuleFactoryCreateData
): Promise<ModuleFactoryResult> {
  return new Promise((resolve, reject) => {
    normalModuleFactory.create(createData, (err, result) => {
      if (err) reject(err);
      else {
        resolve(result as ModuleFactoryResult);
      }
    });
  });
}

// The problem is that it does the recompile but doesn't
// reload Main.wasm.
//
// - it detects the new stuff at the start of webpack
// - it detects the change and does the rebuild
// - but the HMR doesn't realize there's a change.
export default class HaskellPlugin {
  cabalFile: string;
  haskellDirs: string[];
  debugMode: boolean;
  wasmPath: string;
  cabalPath: string;
  hasSetup: boolean;
  wasmContent: Buffer | undefined;
  compiling: Promise<void> = Promise.resolve();
  virtualModulesPlugin = new VirtualModulesPlugin();
  wp: Watchpack = new Watchpack({
  });
  constructor({ cabalFile, haskellDirs, debugMode, wasmPath }: {
    cabalFile: string,
    haskellDirs: string[],
    debugMode: boolean,
    wasmPath: string
  }) {
    this.cabalFile = cabalFile;
    this.haskellDirs = haskellDirs;
    this.cabalPath = process.env["WASM_CABAL"] as string;
    this.hasSetup = false;
    this.debugMode = debugMode;
    this.wasmPath = wasmPath;
  }

  apply(compiler: Compiler) {
    this.virtualModulesPlugin.apply(compiler);
    const pluginName = HaskellPlugin.name;
    const logger = compiler.getInfrastructureLogger(pluginName);
    const { RawSource } = compiler.webpack.sources;
    const debug = (...args: any[]) => {
      if (this.debugMode) {
        logger.info(...args);
      }
    };
    compiler.hooks.beforeCompile.tapPromise(pluginName, async ({ normalModuleFactory }) => {
      debug("Before hook called.");
      if (this.hasSetup) {
        return this.compiling;
      }
      this.hasSetup = true;
      this.wp.watch({
        files: [
          path.resolve(this.cabalFile)
        ],
        directories: [
          ...this.haskellDirs.map(dir => path.resolve(dir))
        ],
        // files assumed to not exist, so no remove
        // event is fired when they aren't there on
        // start.
        missing: [],
        startTime: Date.now() - 10000
      });
      debug("Setting up, beginning initial compile.")
      this.compiling = this._buildHaskellCode(debug, normalModuleFactory);
      let first = true;
      this.wp.on("aggregated", (changes, removals) => {
        debug("Aggregated handler called.", changes, removals);
        if (first) {
          first = false;
        } else {
          debug("Compiling again.");
          this.compiling = this._buildHaskellCode(debug, normalModuleFactory);
        }
      });
      return this.compiling;
    });

    compiler.hooks.thisCompilation.tap(pluginName, (compilation) => {
      /* access information about the finished ffi module
      compilation.hooks.finishModules.tapPromise(this.constructor.name, async (
        modules
      ) => {
        for (let module of modules) {
          if (module instanceof NormalModule && module.userRequest == path.resolve("./haskell/haskell.ffi.js")) {
            //console.info("ffi module", module);
            //console.info("ffi dependencies", module.dependencies);
          }
        }
      });
      */
      compilation.hooks.processAssets.tapPromise(pluginName, async () => {
        let wasmName = "haskell/haskell.wasm";
        let exists = compilation.getAsset(wasmName);
        console.info("WASM FILE exists=",exists);
        let source = new RawSource(this.wasmContent as Buffer);
        if (exists) {
          console.log("update wasm asset");
          compilation.updateAsset(wasmName, source);
        } else {
          console.log("emit wasm asset");
          compilation.emitAsset(wasmName, source);
        }
      });
    });
  }

  async _callCabalBuild(debug): Promise<void> {
    return new Promise((resolve, reject) => {
      const p = spawn(this.cabalPath, ["build"], {
        cwd: import.meta.dirname,
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

  async _buildModules(debug, normalModuleFactory: NormalModuleFactory): Promise<void> {
    let wasmContents = await fs.readFile(this.wasmPath);
    this.wasmContent = wasmContents;
    let ffiContents = await produceFFI(wasmContents);
    // TODO: I could probably create a virutal context for these?
    let ffiFileName = "haskell/haskell.ffi.js";
    let ffiPath = path.resolve(ffiFileName);
    let wasmFileName = "haskell/haskell.wasm";
    let wasmPath = path.resolve(wasmFileName);
    this.virtualModulesPlugin.writeModule(ffiFileName, ffiContents);
    // I'm hoping this will work despite the plugin being written to use
    // a string instead of a buffer.
    this.virtualModulesPlugin.writeModule(wasmFileName, wasmContents as any);
    normalModuleFactory.hooks.createModule.tapPromise(
      this.constructor.name,
      async (
      createData: ResolveCreateData,
      resolveData: ResolveData
    ): Promise<void | Module> => {
      if (createData.request == path.resolve(ffiFileName)) {
        console.log("FFI Module loaders:", createData.loaders);
        console.log("FFI Module encountered, adding dependency", resolveData);
      }
      if (createData.request == path.resolve(wasmFileName)) {
        if (!resolveData?.createData?.settings?.type?.includes?.("wasm")) {
          console.error("The type of this module was not wasm");
          console.log(resolveData);
        }
        console.log("wasm module createData:");
      }
    });
  }

  async _buildHaskellCode(debug, normalModuleFactory: NormalModuleFactory): Promise<void> {
    try {
      await this._callCabalBuild(debug);
      await this._buildModules(debug, normalModuleFactory);
      debug("compilation success.");
    } catch (err) {
      debug("Compilation error", err);
      throw err;
    }
  }
}
