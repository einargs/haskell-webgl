module Component where

import Signal

-- TODO: start implement the signal library.

runtimeMagic = error "magic integrations with runtime system"

-- | A reactive context allows you to read
-- from signals, but you can't write to them.
--
-- This context is useful for embedding things into
-- the elements.
newtype Reactive a = MkReactive { unReactive :: IO a }
  deriving (Functor, Applicative, Monad)

-- | Component is allowed to do things like create
-- signals, but it can't read or set them. It can create
-- memos and other things, but it can't actually access
-- the data inside them or make decisions based on them.
newtype Component a = MkComponent { unComponent :: IO a }
  deriving (Functor, Applicative, Monad)

-- | Effects are sections of code that can read
-- signals but cannot set them. They can then
-- use that to synchronize outside state with
-- the signals.
newtype Effect a = MkEffect { unEffect :: IO a }
  deriving (Functor, Applicative, Monad)

-- TODO: until I write a template transpile thing,
-- just use Miso style stuff.
data ElementKind
  = Div

data AttrValue = AttrValue

newtype AttrName = AttrName JSString

data Attribute
  = StaticAttr AttrName AttrValue
  | DynAttr AttrName (Reactive AttrValue)

data Element
  = HtmlElement ElementKind [Attribute] [Element]
  | EmbededComponent (Component Element)
  | EmbededReactive (Reactive Element)

component :: Component Element -> Element
component = EmbededComponent

reactive :: Reactive Element -> Element
reactive = ReactiveElement

class CanReadSignals m where
  readSignal :: ReadSignal a -> m a

instance CanReadSignals Reactive where
  readSignal rs = runtimeMagic

instance CanReadSignals Effect where
  readSignal rs = runtimeMagic

effectIO :: IO a -> Effect a
effectIO = MkEffect

-- This just embeds the effect into the runtime
-- system as a watcher of the things that
-- effect referenced.
effect :: Effect a -> Component ()
effect eff = runtimeMagic

-- | This function uses synchronous reactivity
-- and monads to create a memoized read only computed
-- signal.
--
-- In react this is a memoized value.
computed :: Reactive a -> Component (ReadSignal a)
computed r = runtimeMagic

-- | different name eventually.
newtype EventHandling a = MkEventHandling { unEventHandling :: IO a }
  deriving (Functor, Applicative, Monad)

data SignalTransState = SignalTransState

-- | Batch multiple signal updates together.
-- Probably implement it as a state monad?
newtype SignalTransaction a = MkSignalTransaction
  { unSignalTransaction :: State SignalTransState a }
  deriving (Functor, Applicative, Monad)

class RunTransaction m where
  runTransaction :: SignalTransaction a -> m a

  updateSignal :: WriteSignal a -> (a -> a) -> m ()
  updateSignal ws f = runTransaction $ updateSignal ws f

  setSignal :: WriteSignal a -> a -> m ()
  setSignal ws f = runTransaction $ setSignal ws f

instance RunTransaction SignalTransaction where
  runTransaction = pure

  -- these are the actual ones that record the state
  -- in the underlying State monad.
  updateSignal = runtimeMagic
  setSignal = runtimeMagic

instance RunTransaction EventHandling where
  runTransaction = runtimeMagic

-- TODO: maybe also allow event handling to dispatch to actions?
-- I think that's a good idea.

