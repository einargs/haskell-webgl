import * as path from 'path';
import Watchpack from 'watchpack';
import { spawn } from 'node:child_process';
import * as fs from 'node:fs/promises';
import { Buffer } from 'node:buffer';
import webpack, { Compiler, Compilation } from 'webpack';
const postLinkModule = await import(process.env["POST_LINK"] as string);

type CompilationParams = Compilation['params'];
type NormalModuleFactory = CompilationParams['normalModuleFactory'];
type ModuleFactoryCreateData = Parameters<NormalModuleFactory['create']>[0];
type ModuleFactoryResult = NonNullable<Parameters<Parameters<NormalModuleFactory['create']>[1]>[1]>;

// Taken from the POST_LINK file that GHC provides.
async function produceFFI(wasmFileSource: Buffer) {
  const { postLink } = postLinkModule;
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
  haskellDir: string;
  debugMode: boolean;
  wasmPath: string;
  cabalPath: string;
  hasSetup: boolean;
  wasmContent: Buffer | undefined;
  compiling: Promise<void>;
  wp: Watchpack;
  constructor({ cabalFile, haskellDir, debugMode, wasmPath }: {
    cabalFile: string,
    haskellDir: string,
    debugMode: boolean,
    wasmPath: string
  }) {
    this.cabalFile = cabalFile;
    this.haskellDir = haskellDir;
    this.cabalPath = process.env["WASM_CABAL"] as string;
    this.hasSetup = false;
    this.debugMode = debugMode;
    this.compiling = Promise.resolve();
    this.wasmPath = wasmPath;
    this.wp = new Watchpack({
    });
  }

  apply(compiler: Compiler) {
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
          path.resolve(this.haskellDir)
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
    compiler.hooks.compilation.tap(pluginName, (compilation) => {
      compilation.hooks.processAssets.tapPromise(pluginName, async () => {
        let updateOrEmit = (name, source) => {
          let exists = compilation.getAsset(name);
          if (exists) {
            compilation.updateAsset(source, name);
          } else {
            compilation.emitAsset(source, name);
          }
        };
        console.info(this.wasmContent?.constructor);
        updateOrEmit(
          "haskell.wasm",
          new RawSource(this.wasmContent as Buffer)
        );
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
    let ffiFileName = "haskell.ffi.js";
    let result = await createModule(normalModuleFactory, {
      contextInfo: {
        issuer: HaskellPlugin.constructor.name,
        compiler: HaskellPlugin.constructor.name,
      },
      resolveOptions: {

      },
      context: "", // will default to the factory context 
      dependencies: [
        //new webpack.dependencies.ModuleDependency("./haskell.wasm")
      ]
    });
    console.log("new", result);
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
