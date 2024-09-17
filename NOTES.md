# TODO
- Finish the View to HtmlElement conversion.
- Figure out why document fragments get used
- Finish the ability to watch things. That means I
  need a way for consumers to have a callback?
  I think I'd rather instead build it so that only
  the watchers register a callback.
- get the ghc-wasm-compat stuff working for better editing


## Mixing Signal Reads and Writes
So we don't want to write to a signal based on a different signal;
that should be done via computed signals. (Maybe I'll need an escape
hatch for when it really is simpler?)

Imagine a scenario where we hit one of three endpoints based on a
user selection, then store the result in a signal. That really
does mean we need to be able to read and write inside the same
stuff, huh? Fuck. I can't even really segregate it.

I also was thinking I should make a tool that lets you turn a
function returning a `CanWrite` or `Reading` into a function
returning an `IO` inside the `Component` monad, but once I
switch to all nodes being `IORef`s that'll be trivial.

Let's just get this working now.

## ghc-wasm-compat
I should be able to simplify the ghc-wasm-compat folder/package
inside https://github.com/konn/ghc-wasm-earthly/tree/main
to be used in my code so that haskell language server works when
I'm using wasm stuff.

Right now the build fails in main because lib/Bind uses the wasm
module. And it'll also error if I use the javascript ffi, so I
need the plugin in that as well.

## Memory Leaks!
There are so many memory leaks in the current implementation that
uses the map. Soooo many.

# Fine Grained Reactivity in Haskell
I want to build a system that produces a free monad with
embeded pieces of reactivity that a runtime system can manage.

This is heavily inspired by leptos-rs, which is inspired by solidJS.

## Notes on how Leptos does Reactivity
Leptos' view exists as a simple data structure -- it doesn't even do the
SwiftUI-style generic composition that I expected. Attributes are enums
that can be a static value or a function that can be run to produce a new
value.

https://docs.rs/leptos_reactive/0.6.15/src/leptos_reactive/signal.rs.html#336-342
`create_signal` hooks into `Runtime::current().create_signal`.

Runtime is here: https://docs.rs/leptos_reactive/0.6.15/src/leptos_reactive/runtime.rs.html#58

What I should also read up on is the signals proposal -- that will have
example code and a very well thought out proposition of all the pieces
you need for fine grained reactivity.

## Haskell design
We will store various things in flat maps that will own them and allow
for mutation ala leptos. Using `Data.HashMap.Strict` is a hash array mapped
trie system, so it will run in constant time effectively.

How do I make sure they get garbage collected then?

I will need a way to get a reference to the real elements? Look at what
the leptos node id does.

I think I want a separate monad that runs inside an event handler versus
creating a component. I want to be able to write to signals and do
side effects inside an event handler. Presumably I'll have something
to do fetches and other things inside an event handler? Actually no,
I should check out how leptos does it with resources.

I think event handlers will have a restricted set of commands, and then
probably an escape hatch to IO. Do I want event handlers to only have
access to setting signals? I guess that's okay.

Actually, I don't think that the `Component` monad should be able to
read or write from signals at all. It's job is produce a static output,
so it shouldn't be able to read dynamic data and fuck that up.

If you want to e.g. render different output, that has to be done via
re-runnable sections of monad inside the elements that are returned.
The leptos closures, in other words.

The Signals proposal has some interesting links to discussions about
async signals, which would be very interesting. I should look at how
solid has approached that. It sounds very similar to react's use and
similar transitions etc. Leptos has some stuff with that too.

## Read up on how leptos does
- resources (for fetch)
- directives
- node ids to interact with the underlying DOM

## Maybe in future
- leptos style actions that you can dispatch to

## NEXT: work on a basic ass haskell impelmentation of sigals
I could implement it myself. Or I could steal from an existing
functional reactive programming library like reactive-bannana
and use that to underlie everything. And then probably write my own
because this project is so much inline with web tech.

I need to read up on functional reactive programming; it would
be a shame to ignore something that haskell is really good at.
But I think it should wait until I have basic signal stuff implemented,
because right now I'm basically just building leptos in haskell.

Also, I want to implement the signals myself to learn.
https://www.youtube.com/watch?time_continue=1&v=mYvkcskJbc4

# Signal Polyfill Notes
I'm basing the reactivity off of the signal-polyfill.

However, the signal polyfill does not cover scheduling. You have
a watcher with a callback that gets invoked synchronously whenever
one of the signals it is watching becomes dirty. You are supposed
to use that to then schedule an actual computation for it.

So an `effect(callback)` implementation would create a computed signal
to track the things used inside `callback`, and then watch would
subscribe to that. Then it would schedule a (e.g. using `queueMicrotask`)
something to go and call `getPending` on the watcher and then call
`get` on all of those signals to force them. (And then `watch` on the
watcher to tell it to reset the pending list.)

I'm not sure I like that approach? I do think that needing to schedule
things will be inevitable in a more advanced framework. So why not
do a quick and dirty one now that could be replaced by something better.

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
