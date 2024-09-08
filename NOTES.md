Useful blog post: https://finley.dev/blog/2024-08-24-ghc-wasm.html

Haskell wasm output to
dist-newstyle/build/wasm32-wasi/ghc-9.11.20240828/haskell-webgl-0.1.0.0/x/haskell-webgl/opt/build/haskell-webgl/haskell-webgl.wasm

# Webpack Notes

## Current Plan
The current plan is to avoid needing to mess with the filesystem
by intercepting calls using resolve hooks on NormalModuleFactory.

I'll use `beforeResolve`, and return the `ResolveData` interface.

If that doesn't work, I'll create a fake file system that that I
tell the created module to use during resolution.

I will use the dependencies to indicate that a 

## With VirtualModulesPlugin
Well, I have the files being generated. But there's still no dependency
on the wasm file. So I think I can make one by hooking into the
beforeModule or other hooks.

I'm going to hook into `createModule` and modify the module before then
creating it.


## Future
Eventually I want to create a generated file that depends on the
ffi and the wasm file, and which handles calling instantiateStreaming
itself.


# Useful Webpack Interfaces
```
declare interface ModuleFactoryCreateData {
	contextInfo: ModuleFactoryCreateDataContextInfo;
	resolveOptions?: ResolveOptions;
	context: string;
	dependencies: Dependency[];
}
declare interface ModuleFactoryCreateDataContextInfo {
	issuer: string;
	issuerLayer?: null | string;
	compiler: string;
}
declare interface ResolveData {
	contextInfo: ModuleFactoryCreateDataContextInfo;
	resolveOptions?: ResolveOptions;
	context: string;
	request: string;
	assertions?: Record<string, any>;
	dependencies: ModuleDependency[];
	dependencyType: string;
	createData: Partial<NormalModuleCreateData & { settings: ModuleSettings }>;
	fileDependencies: LazySet<string>;
	missingDependencies: LazySet<string>;
	contextDependencies: LazySet<string>;

	/**
	 * allow to use the unsafe cache
	 */
	cacheable: boolean;
}
```
