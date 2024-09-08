# Webpack Notes
## Adjusting 
The idea is to avoid needing to mess with the filesystem
by intercepting calls using resolve hooks on NormalModuleFactory.

I'll use `beforeResolve`, and return the `ResolveData` interface.

If that doesn't work, I'll create a fake file system that that I
tell the created module to use during resolution.

I ended up using VirtualModulesPlugin, because they already had
all of the bullshit written.

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

## Checkout Compilation.createModuleAssets
It references a `module.buildInfo.assets` property. Could I use that
to include the wasm asset more neatly?

Is this failing to happen automatically because I have no loader for
the wasm when I indicate the dependency in the ffi loader?


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
