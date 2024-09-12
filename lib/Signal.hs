{-# LANGUAGE TypeFamilies, BlockArguments, RecordWildCards, OverloadedRecordDot, NoFieldSelectors #-}
module Signal () where

import Data.Kind (Type)
import Data.Void (Void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Functor.Identity (Identity(..))
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.These (These(..))
import Control.Monad.State.Strict (
  MonadState(..), modify', StateT, State
  )
import Control.Monad.IO.Class (MonadIO(..))
import Data.Unique.Tag (GCompare(..), GOrdering(..), GEq(..))

import Optics.Lens (Lens', lens)
import Optics.Setter (over, set')
import Optics.Getter (view)

import DependentSlotMap (Key, DSlotMap)
import DependentSlotMap qualified as DSMap

runtimeMagic = error "magic integrations with runtime system"

cycleDetectedError :: a
cycleDetectedError = error "A cycle was detected during computation"

newtype Version = Version Int
  deriving (Show, Eq, Ord)

data SharedData = SharedData
  { version :: Version
  -- ^ Version of the value that this node produces.
  --
  -- This is incremented whenever a new value is produced by this node which is not equal to the
  -- previous value (by whatever definition of equality is in use).
  , lastCleanEpoch :: Version
  -- ^ Epoch at which this node is verified to be clean.
  --
  -- This allows skipping of some polling operations in the case where no signals have been set
  -- since this node was last read.
  , dirty :: Bool
  -- ^ Whether this node (in its consumer capacity) is dirty.
  --
  -- Only live consumers become dirty, when receiving a change notification from a dependency
  -- producer.
{-
  /**
   * Called when the signal becomes "live"
   */
  watched?(): void;

  /**
   * Called when the signal stops being "live"
   */
  unwatched?(): void;
-}
  }
  deriving Show

initialSharedData :: SharedData
initialSharedData = SharedData
  { dirty = False
  , lastCleanEpoch = Version 0
  , version = Version 0
  }

-- | Information about producers that a consumer depends on.
data DependencyOnProducer = DependencyOnProducer
  { producerLastReadVersion :: !Version
  -- ^ `Version` of the value last read by a given producer.
  }

data ConsumerData = ConsumerData
  { producerNodes :: Map ProducerId DependencyOnProducer
  -- ^ Producers that this consumer depends on.

  -- does this really need the nextProducerIndex? I don't think so.

  -- I don't think I need consumerAllowSignalWrites either, because
  -- I'm just banning writing to signals inside computable signals.

{-
  /**
   * Called when a signal is read within this consumer.
   */
  consumerOnSignalRead(node: unknown): void;
-}
  }

initialConsumerData :: ConsumerData
initialConsumerData = ConsumerData
  { producerNodes = Seq.empty
  }

--data DependencyOnConsumer = DependencyOnConsumer {  }

data ProducerData = ProducerData 
  { liveConsumerNodes :: Set ConsumerId
  -- ^ Array of consumers of this producer that are "live" (they require push notifications).
  }

initialProducerData :: ProducerData
initialProducerData = ProducerData
  { liveConsumerNodes = Set.empty
  }

-- |
--
-- I'm pretty sure that I could make this extensible by
-- building this/NodeData as a data family.
data ReactiveNodeKind
  = RwSignalKind
  | ComputedSignalKind
  | WatcherKind

-- | Whether a value in a ComputedSignal is current,
-- hasn't been calculated for the first time, or
-- is currently being recomputed.
data ComputedValue a
  = ComputedValue a
  | UnsetValue
  | ComputingValue

data NodeData (k :: ReactiveNodeKind) (a :: Type) where
  RwSignalData ::
    { rwValue :: a
    -- ^ the current value of this signal.
    , rwEquality :: a -> a -> Bool
    -- ^ Used to check whether a produced value has changed or not.
    } -> NodeData 'RwSignalKind a
  ComputedSignalData :: 
    { cValue :: ComputedValue a
    -- ^ Information about the current value. See 'ComputedValue'.
    , cEquality :: a -> a -> Bool
    -- ^ Used to check whether a produced value has changed or not.
    , computation :: Reading a
    -- ^ The computation that will re-calculate the value using
    -- the producers this signal depends on.
    } -> NodeData 'ComputedSignalKind a
  WatcherData :: NodeData 'ComputedSignalKind Void

-- | A node in the graph of reactive nodes.
data ReactiveNode (k :: ReactiveNodeKind) (a :: Type) where
  RwSignalNode
    :: !(NodeData 'RwSignalKind a)
    -> !SharedData
    -> !ProducerData
    -> !ConsumerData
    -> ReactiveNode 'RwSignalKind a
  ComputedSignalNode
    :: !(NodeData 'ComputedSignalKind a)
    -> !SharedData
    -> !ProducerData
    -> !ConsumerData
    -> ReactiveNode 'ComputedSignalKind a
  WatcherNode
    :: !(NodeData 'WatcherKind Void)
    -> !SharedData
    -> !ConsumerData
    -> ReactiveNode 'WatcherKind Void

sharedDataLens :: forall k a. Lens' (ReactiveNode k a) SharedData
sharedDataLens = lens get set where
  get :: ReactiveNode k a -> SharedData
  get (RwSignalNode nd sd pd cd) = sd
  get (ComputedSignalNode nd sd pd cd) = sd
  get (WatcherNode nd sd cd) = sd
  set :: ReactiveNode k a -> SharedData -> ReactiveNode k a
  set (RwSignalNode rwd sd pd cd) sd' = RwSignalNode rwd sd' pd cd
  set (ComputedSignalNode csd sd pd cd) sd' = ComputedSignalNode csd sd' pd cd
  set (WatcherNode nd sd cd) sd' = WatcherNode nd sd' cd

nodeDataLens :: forall k a. Lens' (ReactiveNode k a) (NodeData k a)
nodeDataLens = lens get set where
  get :: ReactiveNode k a -> NodeData k a
  get (RwSignalNode nd sd pd cd) = nd
  get (ComputedSignalNode nd sd pd cd) = nd
  get (WatcherNode nd sd cd) = nd
  set :: ReactiveNode k a -> NodeData k a -> ReactiveNode k a
  set (RwSignalNode nd sd pd cd) nd' = RwSignalNode nd' sd pd cd
  set (ComputedSignalNode nd sd pd cd) nd' = ComputedSignalNode nd' sd pd cd
  set (WatcherNode nd sd cd) nd' = WatcherNode nd' sd cd

class IsProducer (k :: ReactiveNodeKind) where
  producerDataLens :: forall a. Lens' (ReactiveNode k a) ProducerData
  -- | Tracks whether producers need to recompute their value independently of
  -- the reactive graph (for example, if no initial value has been computed).
  producerMustRecompute :: forall a. ReactiveNode k a -> Bool
  -- | Recompute the value of the producer that the key is for.
  producerRecomputeValue :: forall a. RNKey k a -> SignalRuntimeState -> IO ()

{-
  /**
   * Tracks whether producers need to recompute their value independently of the reactive graph (for
   * example, if no initial value has been computed).
   */
  producerMustRecompute(node: unknown): boolean;
  producerRecomputeValue(node: unknown): void;
  consumerMarkedDirty(this: unknown): void;
-}

class IsConsumer (k :: ReactiveNodeKind) where
  consumerDataLens :: forall a. Lens' (ReactiveNode k a) ConsumerData

instance IsProducer 'RwSignalKind where
  producerDataLens = lens g s where
    g :: ReactiveNode 'RwSignalKind a -> ProducerData
    g (RwSignalNode _nd _sd pd _cd) = pd
    s :: ReactiveNode 'RwSignalKind a -> ProducerData -> ReactiveNode 'RwSignalKind a
    s (RwSignalNode rwd sd _pd cd) pd' = RwSignalNode rwd sd pd' cd
  producerMustRecompute _node = False
  producerRecomputeValue _key _runtime = pure ()

instance IsProducer 'ComputedSignalKind where
  producerDataLens = lens g s where
    g :: ReactiveNode 'ComputedSignalKind a -> ProducerData
    g (ComputedSignalNode _nd _sd pd _cd) = pd
    s :: ReactiveNode 'ComputedSignalKind a -> ProducerData -> ReactiveNode 'ComputedSignalKind a
    s (ComputedSignalNode csd sd _pd cd) pd' = ComputedSignalNode csd sd pd' cd
  producerMustRecompute (ComputedSignalNode csd _sd _pd _cd) =
    -- Is an error for it to be in Computing
    case csd.value of
      ComputingValue -> cycleDetectedError
      UnsetValue -> True
      ComputedValue _ -> False
  producerRecomputeValue key runtime = do
    node <- viewNode runtime key nodeDataLens
    oldValue <- case node.value of
      ComputingValue -> cycleDetectedError
      UnsetValue -> pure Nothing
      ComputedValue a -> pure $ Just a
    adjustNode runtime key nodeDataLens \nd->nd{cValue=ComputingValue}
    let equality = node.cEquality
    newValue <- runReading runtime key node.computation
    let f :: ReactiveNode 'ComputedSignalKind a -> ReactiveNode 'ComputedSignalKind a
        f = over sharedDataLens (\sd -> sd{version=sd.version+1})
          . over nodeDataLens (\nd-> nd{cValue=ComputedValue $ case oldValue of
            Nothing -> newValue
            Just old -> if equality old newValue
              then old else newValue})
    adjustNode runtime key idLens f
    -- consumerAfterComputation
    pure ()


instance IsConsumer 'RwSignalKind where
  consumerDataLens = lens g s where
    g :: ReactiveNode 'RwSignalKind a -> ConsumerData
    g (RwSignalNode _nd _sd _pd cd) = cd
    s :: ReactiveNode 'RwSignalKind a
      -> ConsumerData -> ReactiveNode 'RwSignalKind a
    s (RwSignalNode rwd sd pd _cd) cd' = RwSignalNode rwd sd pd cd'

instance IsConsumer 'ComputedSignalKind where
  consumerDataLens = lens g s where
    g :: ReactiveNode 'ComputedSignalKind a -> ConsumerData
    g (ComputedSignalNode _nd _sd _pd cd) = cd
    s :: ReactiveNode 'ComputedSignalKind a
      -> ConsumerData -> ReactiveNode 'ComputedSignalKind a
    s (ComputedSignalNode csd sd pd _cd) cd' = ComputedSignalNode csd sd pd cd'

instance IsConsumer 'WatcherKind where
  consumerDataLens = lens g s where
    g :: ReactiveNode 'WatcherKind a -> ConsumerData
    g (WatcherNode _nd _sd cd) = cd
    s :: ReactiveNode 'WatcherKind a
      -> ConsumerData -> ReactiveNode 'WatcherKind a
    s (WatcherNode csd sd _cd) cd' = WatcherNode csd sd cd'

idLens :: Lens' a a
idLens = lens id const

data RNKey (k :: ReactiveNodeKind) (a :: Type) where
  RwSignal :: Key IO (ReactiveNode 'RwSignalKind a) -> RNKey 'RwSignalKind a
  ComputedSignal :: Key IO (ReactiveNode 'ComputedSignalKind a) -> RNKey 'ComputedSignalKind a
  Watcher :: Key IO (ReactiveNode 'WatcherKind Void) -> RNKey 'WatcherKind Void

toBaseKey :: RNKey k a -> Key IO (ReactiveNode k a)
toBaseKey (RwSignal key) = key
toBaseKey (ComputedSignal key) = key
toBaseKey (Watcher key) = key

type RwSignal = RNKey 'RwSignalKind
type ComputedSignal = RNKey 'ComputedSignalKind
type Watcher = RNKey 'WatcherKind Void

data ConsumerId = forall a k. IsConsumer k => ConsumerId (RNKey k a)

instance Eq ConsumerId where
  (ConsumerId a) == (ConsumerId b) = case geq ak bk of
    Nothing -> False
    Just _refl -> True
    where
    ak = toBaseKey a
    bk = toBaseKey b

instance Ord ConsumerId where
  compare (ConsumerId a) (ConsumerId b) = case gcompare ak bk of
    GLT -> LT
    GEQ -> EQ
    GGT -> GT
    where
    ak = toBaseKey a
    bk = toBaseKey b

data ProducerId = forall a k. IsProducer k => ProducerId (RNKey k a)

instance Eq ProducerId where
  (ProducerId a) == (ProducerId b) = case geq ak bk of
    Nothing -> False
    Just _refl -> True
    where
    ak = toBaseKey a
    bk = toBaseKey b

instance Ord ProducerId where
  compare (ProducerId a) (ProducerId b) = case gcompare ak bk of
    GLT -> LT
    GEQ -> EQ
    GGT -> GT
    where
    ak = toBaseKey a
    bk = toBaseKey b

-- | I'm pretty sure that I'm going to need to either
-- figure out how to make this a weak set or better yet
-- build some smart handling to remove consumers? Wait,
-- isn't that built in with the liveness? I can also probably
-- track what's in a component and deallocate it when that
-- component goes away? Does that make sense given how components
-- work?
type RNGraph = DSlotMap IO

data SignalRuntimeState = SignalRuntimeState
  { nodeGraph :: !RNGraph
  }

-- | Read a property of the node through a lens.
viewNode :: forall k v a m.
  MonadIO m => SignalRuntimeState -> RNKey k v -> Lens' (ReactiveNode k v) a -> m a
viewNode SignalRuntimeState{nodeGraph} key lens = liftIO $ do
  node <- DSMap.lookup (toBaseKey key) nodeGraph
  case node of
    Nothing -> error "Node did not exist corresponding to key"
    Just node' -> pure $ view lens node'

-- | Update a property of the node through a lens.
setNode
  :: MonadIO m
  => SignalRuntimeState
  -> RNKey k v
  -> Lens' (ReactiveNode k v) a
  -> a
  -> m ()
setNode SignalRuntimeState{nodeGraph} key lens val =
  liftIO $ DSMap.adjust (set' lens val) (toBaseKey key) nodeGraph

-- | Apply a modifying function to the node through a lens.
adjustNode
  :: MonadIO m
  => SignalRuntimeState
  -> RNKey k v
  -> Lens' (ReactiveNode k v) a
  -> (a -> a)
  -> m ()
adjustNode SignalRuntimeState{nodeGraph} key lens f =
  liftIO $ DSMap.adjust (over lens f) (toBaseKey key) nodeGraph

-- | Creates a read-write signal that uses the built-in
-- equality typeclass to determine if different.
--
-- I'll implement a second function that lets you customize equality.
createRwSignalNode
  :: forall m a. (MonadIO m, Eq a)
  => SignalRuntimeState -> a -> m (RwSignal a)
createRwSignalNode SignalRuntimeState{nodeGraph} initialValue = liftIO $ do
  (key :: Key IO (ReactiveNode 'RwSignalKind a)) <- DSMap.insert node nodeGraph
  pure $ RwSignal key
  where
    node = RwSignalNode
      (RwSignalData initialValue (==))
      initialSharedData initialProducerData initialConsumerData

-- | Creates a read-write signal that uses the built-in
-- equality typeclass to determine if different.
--
-- I'll implement a second function that lets you customize equality.
createComputedSignalNode
  :: forall m a. (MonadIO m, Eq a)
  => SignalRuntimeState -> Reading a -> m (ComputedSignal a)
createComputedSignalNode SignalRuntimeState{nodeGraph} computation = liftIO do
  key <- DSMap.insert node nodeGraph
  pure $ ComputedSignal key
  where
    node = ComputedSignalNode
      (ComputedSignalData UnsetValue (==) computation)
      initialSharedData
      initialProducerData
      initialConsumerData
  

data ReadingState = ReadingState
  { usedProducers :: Set ProducerId
  , runtime :: SignalRuntimeState
  }

-- | Can only read from signals.
--
-- Used for e.g. creating new computed signals.
newtype Reading a = MkReading { unReading :: StateT ReadingState IO a }
  deriving (Functor, Applicative, Monad, MonadState ReadingState, MonadIO)

producerAccessed :: IsProducer k => RNKey k a -> Reading ()
producerAccessed key = do
  let pid = ProducerId key
  modify' $ \st -> st{usedProducers=Set.insert pid st.usedProducers}

readRwSignal :: RwSignal a -> Reading a
readRwSignal signal = do
  producerAccessed signal
  reading@ReadingState{runtime} <- get
  signalData <- viewNode @'RwSignalKind runtime signal nodeDataLens
  pure $ case signalData of
    RwSignalData{rwValue} -> rwValue

-- computedProducerMustRecompute :: 
-- computedProducerRecomputeValue ::

producerUpdateValueVersion
  :: (MonadIO m, IsProducer k)
  => SignalRuntimeState -> RNKey k a -> m (ReactiveNode k a)
producerUpdateValueVersion signal = do
  
  error "todo"

readComputedSignal :: ComputedSignal a -> Reading a
readComputedSignal signal = do
  ReadingState{runtime} <- get
  node <- producerUpdateValueVersion runtime signal
  let nodeData = get nodeDataLens node
  pure $ nodeData.cValue

runReading :: (IsConsumer k,  MonadIO m)
  => SignalRuntimeState -> RNKey k a -> Reading a -> m a
runReading runtime activeConsumer m = liftIO do
  error "todo"










{-
-- | See producerRemoveLiveConsumerAtIndex in graph.ts
--
-- The javascript asserts that the producer is also a consumer, but
-- I don't see any reason to do that here.
producerRemoveLiveConsumer
  :: (IsProducer k1, IsConsumer k2)
  => SignalRuntimeState -> RNKey k1 a -> RNKey k2 b -> IO ()
producerRemoveLiveConsumer runtime producer consumer = do
  -- don't need to update the backpointing indexes because we're
  -- using sets and ids
  pure ()

-- | see graph.ts for implementation of producerAccessed
producerAccessed :: IsProducer k => ReadingState -> RNKey k a -> IO ()
producerAccessed reading key = do
  -- use a hashmap of producer keys to the producer dependency
  -- info. That way I can also avoid tracking the index.
  -- and that way I don't do the weird walk through the array.
  --
  -- wait, no, I think I do need the index to allow coordination
  -- across multiple producers for the same consumer?
  --
  -- No -- the way this is designed I can always build a list in the
  -- ReadingMonad of all the producers a consumer depends on.
  -- So that means I need to write a version of that gets handed
  -- a set of all the producers at the end of running a read only monad.
  pure ()
-}


{-

-- | Context that can create signals and memos, but can't
-- read or set them.
--
-- Does this need to be a monad? Can I get more interesting
-- stuff by using applicative do.
newtype CreateOnly a = MkCreateOnly { unCreateOnly :: IO a }
  deriving (Functor, Applicative)

-- The ids go to a slotmap ala leptos that can be
-- mutated to deal with the signal graph.

data ReadSignal a = ReadSignal ReadSignalId
data WriteSignal a = WriteSignal WriteSignalId

createSignal :: a -> CreateOnly (ReadSignal a, WriteSignal a)
createSignal defaultValue = runtimeMagic

data TransState = TransState

-- | Batch multiple signal updates together.
--
-- Can only mutate signals, not read them. It can do
-- updates that refer to the previous version.
newtype Transaction a = MkTransaction
  { unTransaction :: State TransState a }
  deriving (Functor, Applicative, Monad)

updateSignal :: WriteSignal a -> (a -> a) -> Transaction ()
updateSignal ws f = runtimeMagic

setSignal :: WriteSignal a -> a -> Transaction ()
setSignal ws v = runtimeMagic
-}
