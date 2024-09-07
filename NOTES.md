Useful blog post: https://finley.dev/blog/2024-08-24-ghc-wasm.html

Haskell wasm output to
dist-newstyle/build/wasm32-wasi/ghc-9.11.20240828/haskell-webgl-0.1.0.0/x/haskell-webgl/opt/build/haskell-webgl/haskell-webgl.wasm

# Webpack Notes
## Using jsconfig.json to get type info from JSDoc comments

## NormalModuleFactory.create
concrete implementation of ModuleFactory.create
```
	 * @param {ModuleFactoryCreateData} data data object
	 * @param {function((Error | null)=, ModuleFactoryResult=): void} callback callback
	 * @returns {void}
	 */
	create(data, callback)
```

```
 * @typedef {object} ModuleFactoryResult
 * @property {Module=} module the created module or unset if no module was created
 * @property {Set<string>=} fileDependencies
 * @property {Set<string>=} contextDependencies
 * @property {Set<string>=} missingDependencies
 * @property {boolean=} cacheable allow to use the unsafe cache

 * @typedef {object} ModuleFactoryCreateDataContextInfo
 * @property {string} issuer
 * @property {string | null=} issuerLayer
 * @property {string} compiler

 * @typedef {object} ModuleFactoryCreateData
 * @property {ModuleFactoryCreateDataContextInfo} contextInfo
 * @property {ResolveOptions=} resolveOptions
 * @property {string} context
 * @property {Dependency[]} dependencies
```
