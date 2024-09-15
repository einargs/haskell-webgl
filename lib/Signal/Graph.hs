{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}

module Signal.Graph (

) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict
  ( MonadState (..),
    State,
    StateT,
    modify',
    runStateT,
  )
import Data.Functor.Identity (Identity (..))
import Data.Functor ((<&>))
import Data.Kind (Type, Constraint)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.These (These (..))
import Data.Unique.Tag (GCompare (..), GEq (..), GOrdering (..))
import Data.Void (Void)
import DependentSlotMap (DSlotMap, Key)
import DependentSlotMap qualified as DSMap
import GHC.Generics (Generic)
import Data.Foldable (for_)

import Optics.Each (each)
import Optics.Indexed (ifor_)
import Optics.Getter (view)
import Optics.IxTraversal (iforOf)
import Optics.Label (LabelOptic (..))
import Optics.Lens (A_Lens, Lens', lens, lensVL)
import Optics.Operators ((%~), (.~), (^.))
import Optics.Optic ((%))
import Optics.Setter (over, set')

runtimeMagic = error "magic integrations with runtime system"

cycleDetectedError :: a
cycleDetectedError = error "A cycle was detected during computation"

newtype Version = Version Int
  deriving (Show, Eq, Ord)

bumpVersion :: Version -> Version
bumpVersion (Version i) = Version $ i + 1

data SharedData = SharedData
  { -- | Version of the value that this node produces.
    --
    -- This is incremented whenever a new value is produced by this node which is not equal to the
    -- previous value (by whatever definition of equality is in use).
    version :: Version,
    -- | Epoch at which this node is verified to be clean.
    --
    -- This allows skipping of some polling operations in the case where no signals have been set
    -- since this node was last read.
    lastCleanEpoch :: Version,
    -- | Whether this node (in its consumer capacity) is dirty.
    --
    -- Only live consumers become dirty, when receiving a change notification from a dependency
    -- producer.
    dirty :: Bool
  }
  deriving (Show, Generic)

initialSharedData :: SharedData
initialSharedData =
  SharedData
    { dirty = False,
      lastCleanEpoch = Version 0,
      version = Version 0
    }

-- | Information about producers that a consumer depends on.
data DependencyOnProducer = DependencyOnProducer
  { -- | `Version` of the value last read by a given producer.
    lastReadVersion :: !Version
  }
  deriving (Generic)

type ProducerNodeDeps = Map ProducerId DependencyOnProducer

data ConsumerData = ConsumerData
  { -- | Producers that this consumer depends on.
    producerNodes :: ProducerNodeDeps
  }
  -- This doesn't need nextProducerIndex because we are using
  -- ids and sets. Less efficient, but I need to do an all IORef
  -- version to make it efficient.

  -- I don't think I need consumerAllowSignalWrites either, because
  -- I'm just banning writing to signals inside computable signals.
  -- The monad won't allow it.

  deriving (Generic)

initialConsumerData :: ConsumerData
initialConsumerData =
  ConsumerData
    { producerNodes = Map.empty
    }

-- data DependencyOnConsumer = DependencyOnConsumer {  }

data ProducerData = ProducerData
  { liveConsumerNodes :: Set ConsumerId
    -- ^ Array of consumers of this producer that are "live"
    -- (they require push notifications).
  } deriving Generic

{-
Watched and unwatched are options passed when creating
a signal in the pollyfill wrapper.ts thing. They're hooks
to observe the behavior, but I don't think we need/want
them in this implementation. They're meant for when you're
building your own reactive system on top of it, but here
this reactive system is fully integrated.

  /**
   * Called when the signal becomes "live"
   */
  watched?(): void;

  /**
   * Called when the signal stops being "live"
   */
  unwatched?(): void;
-}

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

-- TODO: replace this with a data/type family indexed on 'ReactiveNodeKind'
-- so I can auto-derive the lenses.
data NodeData (k :: ReactiveNodeKind) (a :: Type) where
  RwSignalData ::
    { -- | the current value of this signal.
      rwValue :: a,
      -- | Used to check whether a produced value has changed or not.
      rwEquality :: a -> a -> Bool
    } ->
    NodeData 'RwSignalKind a
  ComputedSignalData ::
    { -- | Information about the current value. See 'ComputedValue'.
      cValue :: ComputedValue a,
      -- | Used to check whether a produced value has changed or not.
      cEquality :: a -> a -> Bool,
      -- | The computation that will re-calculate the value using
      -- the producers this signal depends on.
      computation :: Reading a
    } ->
    NodeData 'ComputedSignalKind a
  WatcherData :: NodeData 'WatcherKind Void

instance LabelOptic "rwValue" A_Lens (NodeData 'RwSignalKind a) (NodeData 'RwSignalKind a) a a where
  labelOptic = lensVL \f signalData@(RwSignalData {..}) ->
    let g a = signalData {rwValue = a}
     in g <$> f rwValue

instance
  LabelOptic
    "rwEquality"
    A_Lens
    (NodeData 'RwSignalKind a)
    (NodeData 'RwSignalKind a)
    (a -> a -> Bool)
    (a -> a -> Bool)
  where
  labelOptic = lensVL \f signalData@(RwSignalData {..}) ->
    let g a = signalData {rwEquality = a}
     in g <$> f rwEquality

instance
  LabelOptic
    "cValue"
    A_Lens
    (NodeData 'ComputedSignalKind a)
    (NodeData 'ComputedSignalKind a)
    (ComputedValue a)
    (ComputedValue a)
  where
  labelOptic = lensVL \f signalData@(ComputedSignalData {..}) ->
    let g a = signalData {cValue = a}
     in g <$> f cValue

instance
  LabelOptic
    "cEquality"
    A_Lens
    (NodeData 'ComputedSignalKind a)
    (NodeData 'ComputedSignalKind a)
    (a -> a -> Bool)
    (a -> a -> Bool)
  where
  labelOptic = lensVL \f signalData@(ComputedSignalData {..}) ->
    let g a = signalData {cEquality = a}
     in g <$> f cEquality

instance
  LabelOptic
    "computation"
    A_Lens
    (NodeData 'ComputedSignalKind a)
    (NodeData 'ComputedSignalKind a)
    (Reading a)
    (Reading a)
  where
  labelOptic = lensVL \f signalData@(ComputedSignalData {..}) ->
    let g a = signalData {computation = a}
     in g <$> f computation

-- | A node in the graph of reactive nodes.
--
-- I'm still not sure if RwSignals are actually consumers or
-- not. I don't think they would be, but the polyfill is kind
-- of confusing on it. I'll remove it for now
data ReactiveNode (k :: ReactiveNodeKind) (a :: Type) where
  RwSignalNode ::
    !(NodeData 'RwSignalKind a) ->
    !SharedData ->
    !ProducerData ->
    -- -> !ConsumerData
    ReactiveNode 'RwSignalKind a
  ComputedSignalNode ::
    !(NodeData 'ComputedSignalKind a) ->
    !SharedData ->
    !ProducerData ->
    !ConsumerData ->
    ReactiveNode 'ComputedSignalKind a
  WatcherNode ::
    !(NodeData 'WatcherKind Void) ->
    !SharedData ->
    !ConsumerData ->
    ReactiveNode 'WatcherKind Void

sharedDataLens :: forall k a. Lens' (ReactiveNode k a) SharedData
sharedDataLens = lens g s
  where
    g :: ReactiveNode k a -> SharedData
    g (RwSignalNode _nd sd _pd) = sd
    g (ComputedSignalNode _nd sd _cd _pd) = sd
    g (WatcherNode _nd sd _cd) = sd
    s :: ReactiveNode k a -> SharedData -> ReactiveNode k a
    s (RwSignalNode rwd _sd pd) sd' = RwSignalNode rwd sd' pd
    s (ComputedSignalNode csd _sd pd cd) sd' = ComputedSignalNode csd sd' pd cd
    s (WatcherNode nd _sd cd) sd' = WatcherNode nd sd' cd

nodeDataLens :: forall k a. Lens' (ReactiveNode k a) (NodeData k a)
nodeDataLens = lens g s
  where
    g :: ReactiveNode k a -> NodeData k a
    g (RwSignalNode nd sd pd) = nd
    g (ComputedSignalNode nd sd pd cd) = nd
    g (WatcherNode nd sd cd) = nd
    s :: ReactiveNode k a -> NodeData k a -> ReactiveNode k a
    s (RwSignalNode nd sd pd) nd' = RwSignalNode nd' sd pd
    s (ComputedSignalNode nd sd pd cd) nd' = ComputedSignalNode nd' sd pd cd
    s (WatcherNode nd sd cd) nd' = WatcherNode nd' sd cd

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

  -- | Check if a consumer is live.
  --
  -- Some consumers are always live, like a watcher; others, like
  -- computed signals, are only live consumers when they as producers
  -- are subscribed to by other consumers.
  consumerIsLive :: forall a. ReactiveNode k a -> Bool

-- | This implementation of 'consumerIsLive' is used for
-- when a consumer is always live -- like a watcher.
consumerIsAlwaysLiveImpl :: ReactiveNode k a -> Bool
consumerIsAlwaysLiveImpl = const True

-- | The standard implementation for
producerConsumerIsLiveImpl :: (IsProducer k) => ReactiveNode k a -> Bool
producerConsumerIsLiveImpl node =
  (node ^. producerDataLens % #liveConsumerNodes) /= Set.empty

{-
  /**
   * Called when a signal is read within this consumer.
   */
  consumerOnSignalRead(node: unknown): void;
-}

instance IsProducer 'RwSignalKind where
  producerDataLens = lens g s
    where
      g :: ReactiveNode 'RwSignalKind a -> ProducerData
      g (RwSignalNode _nd _sd pd) = pd
      s :: ReactiveNode 'RwSignalKind a -> ProducerData -> ReactiveNode 'RwSignalKind a
      s (RwSignalNode rwd sd _pd) pd' = RwSignalNode rwd sd pd'
  producerMustRecompute _node = False
  producerRecomputeValue _key _runtime = pure ()

instance IsProducer 'ComputedSignalKind where
  producerDataLens = lens g s
    where
      g :: ReactiveNode 'ComputedSignalKind a -> ProducerData
      g (ComputedSignalNode _nd _sd pd _cd) = pd
      s :: ReactiveNode 'ComputedSignalKind a -> ProducerData -> ReactiveNode 'ComputedSignalKind a
      s (ComputedSignalNode csd sd _pd cd) pd' = ComputedSignalNode csd sd pd' cd
  producerMustRecompute (ComputedSignalNode csd _sd _pd _cd) =
    -- Is an error for it to be in Computing
    case csd ^. #cValue of
      ComputingValue -> cycleDetectedError
      UnsetValue -> True
      ComputedValue _ -> False
  producerRecomputeValue :: forall a. ComputedSignal a -> SignalRuntimeState -> IO ()
  producerRecomputeValue key runtime = do
    (node :: ReactiveNode 'ComputedSignalKind a) <- viewNode runtime key
    oldValue <- case node ^. nodeDataLens % #cValue of
      -- computation of this somehow lead to a cyclic read of this.
      ComputingValue -> cycleDetectedError
      UnsetValue -> pure Nothing
      ComputedValue a -> pure $ Just a
    adjustNode runtime key $ set' (nodeDataLens % #cValue) ComputingValue
    let equality = node ^. nodeDataLens % #cEquality
    newValue <- runReading runtime key $ node ^. nodeDataLens % #computation
    let nextValue = case oldValue of
          Nothing -> newValue
          Just old ->
            if equality old newValue
              then old
              else newValue
        f :: ReactiveNode 'ComputedSignalKind a -> ReactiveNode 'ComputedSignalKind a
        f =
          (sharedDataLens % #version %~ bumpVersion)
            . (nodeDataLens % #cValue .~ ComputedValue nextValue)
    adjustNode runtime key f

{-
instance IsConsumer 'RwSignalKind where
  consumerDataLens = lens g s where
    g :: ReactiveNode 'RwSignalKind a -> ConsumerData
    g (RwSignalNode _nd _sd _pd cd) = cd
    s :: ReactiveNode 'RwSignalKind a
      -> ConsumerData -> ReactiveNode 'RwSignalKind a
    s (RwSignalNode rwd sd pd _cd) cd' = RwSignalNode rwd sd pd cd'
-}

instance IsConsumer 'ComputedSignalKind where
  consumerDataLens = lens g s
    where
      g :: ReactiveNode 'ComputedSignalKind a -> ConsumerData
      g (ComputedSignalNode _nd _sd _pd cd) = cd
      s ::
        ReactiveNode 'ComputedSignalKind a ->
        ConsumerData ->
        ReactiveNode 'ComputedSignalKind a
      s (ComputedSignalNode csd sd pd _cd) cd' = ComputedSignalNode csd sd pd cd'

  consumerIsLive = producerConsumerIsLiveImpl

instance IsConsumer 'WatcherKind where
  consumerDataLens = lens g s
    where
      g :: ReactiveNode 'WatcherKind a -> ConsumerData
      g (WatcherNode _nd _sd cd) = cd
      s ::
        ReactiveNode 'WatcherKind a ->
        ConsumerData ->
        ReactiveNode 'WatcherKind a
      s (WatcherNode csd sd _cd) cd' = WatcherNode csd sd cd'

  consumerIsLive = consumerIsAlwaysLiveImpl

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

data ConsumerId = forall a k. (IsConsumer k) => ConsumerId (RNKey k a)

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

data ProducerId = forall a k. (IsProducer k) => ProducerId (RNKey k a)

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

data RNKindSing (k :: ReactiveNodeKind) where
  RwSignalSing :: RNKindSing 'RwSignalKind
  ComputedSignalSing :: RNKindSing 'ComputedSignalKind
  WatcherSing :: RNKindSing 'WatcherKind

-- |
--
-- I could adjust some of the other things to let me
-- implement this for only e.g. ReactiveNode and have
-- RNKey and NodeData have their versions automatically derived
-- as well.
--
-- I don't know how I would automate creating
-- 'AsProducer' and 'AsConsumer' instances though.
class HasRNKind (f :: ReactiveNodeKind -> Type -> Type) where
  getRNKindSing :: f k a -> RNKindSing k

instance HasRNKind RNKey where
  getRNKindSing RwSignal{} = RwSignalSing
  getRNKindSing ComputedSignal{} = ComputedSignalSing
  getRNKindSing Watcher{} = WatcherSing

instance HasRNKind ReactiveNode where
  getRNKindSing RwSignalNode{} = RwSignalSing
  getRNKindSing ComputedSignalNode{} = ComputedSignalSing
  getRNKindSing WatcherNode{} = WatcherSing

data Witness (c :: ReactiveNodeKind -> Constraint) (k :: ReactiveNodeKind)
  = c k => Witness

asProducer :: HasRNKind f => f k a -> Maybe (Witness IsProducer k)
asProducer f = case getRNKindSing f of
  RwSignalSing -> Just Witness
  ComputedSignalSing -> Just Witness
  WatcherSing -> Nothing

asConsumer :: HasRNKind f => f k a -> Maybe (Witness IsConsumer k)
asConsumer f = case getRNKindSing f of
  RwSignalSing -> Nothing
  ComputedSignalSing -> Just Witness
  WatcherSing -> Just Witness

-- Note that instead of defining a special withWitnessM style
-- function, we can just use 'for_'.

-- | I'm pretty sure that I'm going to need to either
-- figure out how to make this a weak set or better yet
-- build some smart handling to remove consumers? Wait,
-- isn't that built in with the liveness? I can also probably
-- track what's in a component and deallocate it when that
-- component goes away? Does that make sense given how components
-- work?
--
-- I could just use IORefs for each individual signal couldn't
-- I? That would be much more efficient. Damn it.
--
-- It also wouldn't have the problem of how to get rid of old
-- nodes.
type RNGraph = DSlotMap IO

data SignalRuntimeState = SignalRuntimeState
  { nodeGraph :: !RNGraph,
    currentEpoch :: Version
  }
  deriving (Generic)

-- | Read a property of the node through a lens.
viewNode ::
  forall k v a m.
  (MonadIO m) => SignalRuntimeState -> RNKey k v -> m (ReactiveNode k v)
viewNode SignalRuntimeState {nodeGraph} key = liftIO do
  node <- DSMap.lookup (toBaseKey key) nodeGraph
  case node of
    Nothing -> error "Node did not exist corresponding to key"
    Just node' -> pure node'

-- | Apply a modifying function to the node.
adjustNode ::
  (MonadIO m) =>
  SignalRuntimeState ->
  RNKey k v ->
  (ReactiveNode k v -> ReactiveNode k v) ->
  m ()
adjustNode SignalRuntimeState {nodeGraph} key f =
  liftIO $ DSMap.adjust f (toBaseKey key) nodeGraph

-- | Creates a read-write signal that uses the built-in
-- equality typeclass to determine if different.
--
-- I'll implement a second function that lets you customize equality.
createRwSignalNode ::
  forall m a.
  (MonadIO m, Eq a) =>
  SignalRuntimeState -> a -> m (RwSignal a)
createRwSignalNode SignalRuntimeState {nodeGraph} initialValue = liftIO $ do
  (key :: Key IO (ReactiveNode 'RwSignalKind a)) <- DSMap.insert node nodeGraph
  pure $ RwSignal key
  where
    node =
      RwSignalNode
        (RwSignalData initialValue (==))
        initialSharedData
        initialProducerData

-- | Creates a read-write signal that uses the built-in
-- equality typeclass to determine if different.
--
-- I'll implement a second function that lets you customize equality.
createComputedSignalNode ::
  forall m a.
  (MonadIO m, Eq a) =>
  SignalRuntimeState -> Reading a -> m (ComputedSignal a)
createComputedSignalNode SignalRuntimeState {nodeGraph} computation = liftIO do
  key <- DSMap.insert node nodeGraph
  pure $ ComputedSignal key
  where
    node =
      ComputedSignalNode
        (ComputedSignalData UnsetValue (==) computation)
        initialSharedData
        initialProducerData
        initialConsumerData

data ReadingState = ReadingState
  { usedProducers :: Map ProducerId DependencyOnProducer,
    runtime :: SignalRuntimeState
  }
  deriving (Generic)

-- | Can only read from signals.
--
-- Used for e.g. creating new computed signals.
newtype Reading a = MkReading {unReading :: StateT ReadingState IO a}
  deriving (Functor, Applicative, Monad, MonadState ReadingState, MonadIO)

-- | Record that this producer was accessed along with the
-- version it was accessed at into ReadingState.usedProducers.
producerAccessed :: (IsProducer k) => RNKey k a -> Reading ()
producerAccessed key = do
  ReadingState {runtime} <- get
  node <- viewNode runtime key
  let pid = ProducerId key
      version = node ^. sharedDataLens % #version
      dep = DependencyOnProducer {lastReadVersion = version}
  modify' $ #usedProducers %~ Map.insert pid dep

readRwSignal :: RwSignal a -> Reading a
readRwSignal signal = do
  reading@ReadingState {runtime} <- get
  producerAccessed signal
  signalData <- viewNode @'RwSignalKind runtime signal
  pure $ case signalData ^. nodeDataLens of
    RwSignalData {rwValue} -> rwValue

-- | Poll the producers this consumer depends on to see if there has
-- been any changes. Returns true if there have been changes.
consumerPollProducersForChange ::
  (IsConsumer k, MonadIO m) =>
  SignalRuntimeState -> RNKey k a -> m Bool
consumerPollProducersForChange runtime signal = do
  node <- viewNode runtime signal
  -- NOTE: this does not short circuit and thus could be improved
  or <$> iforOf
    each
    (node ^. consumerDataLens % #producerNodes)
    \(ProducerId producerKey) dep -> do
      let seenVersion = dep.lastReadVersion
          getProducerVersion = do
            producerNode <- viewNode runtime producerKey
            pure $ producerNode ^. sharedDataLens % #version
      pVersion <- getProducerVersion
      -- a mismatch means that the producer's value is known
      -- to have changed since the last time we saw it.
      if pVersion == seenVersion
        then pure False
        else do
          -- The producer's version is the same as the last time we
          -- read it, but it might itself be stale. Force the producer
          -- to recompute its version (calculating a new value if
          -- necessary).
          producerUpdateValueVersion runtime producerKey
          -- Then repeat the check with the updated version.
          (seenVersion /=) <$> getProducerVersion

-- | Ensure that this producer's `version` is up to date.
producerUpdateValueVersion ::
  (MonadIO m, IsProducer k) =>
  SignalRuntimeState -> RNKey k a -> m ()
producerUpdateValueVersion runtime signal = do
  let SignalRuntimeState {currentEpoch} = runtime
  node <- viewNode runtime signal
  let sharedData = node ^. sharedDataLens
  -- A live consumer will be marked dirty by producers, so a clean
  -- state means that the version is guarenteed to be up to date.
  let liveAndClean = flip (maybe False) (asConsumer node)
        \Witness -> consumerIsLive node && not sharedData.dirty
  -- Even non-live consumers can skip polling if they previously
  -- found themselves to be clean at the current epoch, since their
  -- dependencies could not possibly have changed (such a change
  -- would have increased the epoch).
  let cleanThisEpoch = not sharedData.dirty && sharedData.lastCleanEpoch == currentEpoch
  when (not liveAndClean && not cleanThisEpoch) do
    let mustRecompute = producerMustRecompute node
    producersChanged <- flip (maybe (pure False)) (asConsumer signal) \Witness ->
      consumerPollProducersForChange runtime signal
    -- If either the node says we need to recompute or one of the producers
    -- for this node says a producer we depend on has changed, then we
    -- need to recompute.
    when (mustRecompute || producersChanged) $
      liftIO $ producerRecomputeValue signal runtime
    -- We are now no longer dirty and can update our last clean
    -- epoch.
    adjustNode
      runtime
      signal
      ( over
          sharedDataLens
          (set' #dirty False . set' #lastCleanEpoch currentEpoch)
      )

readComputedSignal :: forall a. ComputedSignal a -> Reading a
readComputedSignal signal = do
  ReadingState {runtime} <- get
  producerUpdateValueVersion runtime signal
  producerAccessed signal
  node <- viewNode runtime signal
  case node ^. nodeDataLens % #cValue of
    ComputedValue v -> pure v
    _ -> error "computed signal failed to resolve to a value"

-- |
--
-- This acts like the polyfill versions of both
-- consumerAfterComputation and producerAccessed.
runReading ::
  (IsConsumer k, MonadIO m) =>
  SignalRuntimeState -> RNKey k a -> Reading a -> m a
runReading runtime activeConsumer m = liftIO do
  let startState =
        ReadingState
          { runtime = runtime,
            usedProducers = Map.empty
          }
  -- we run the computation
  (computedValue, endState) <- flip runStateT startState $ m.unReading
  let ReadingState {runtime = runtime', usedProducers} = endState
  consumerNode <- viewNode runtime activeConsumer
  let isThisLive = consumerIsLive consumerNode
      oldProducers = consumerNode ^. consumerDataLens % #producerNodes

  -- we need to figure out what producers are being removed
  -- and remove the activeConsumer from their list using
  -- removeConsumerFromProducer.
  let removedProducers = oldProducers Map.\\ usedProducers
  ifor_ removedProducers \(ProducerId producer) _dep -> do
    removeConsumerFromProducer runtime activeConsumer producer

  -- we set the current producers of this consumer
  adjustNode runtime activeConsumer $
    consumerDataLens % #producerNodes .~ usedProducers

  -- Then we need to figure out the new producers and
  -- if this consumer is live add it to the live consumer lists
  -- of the producers.
  when isThisLive do
    let addedProducers = usedProducers Map.\\ oldProducers
    ifor_ addedProducers \(ProducerId producer) _dep -> do
      producerAddLiveConsumer runtime producer activeConsumer

  -- Finally we return the new computed value.
  pure computedValue

-- | Add the consumer as a live consumer of this producer.
--
-- Note that this operatiion is potentially transitive. If this node
-- becomes live, then it becomes a live consumer of all of the producers
-- it depends on.
producerAddLiveConsumer ::
  (IsProducer p, IsConsumer c, MonadIO m) =>
  SignalRuntimeState -> RNKey p a -> RNKey c b -> m ()
producerAddLiveConsumer runtime producer consumer = do
  -- we check if this node is a consumer.
  for_ (asConsumer producer) \Witness -> do
    node <- viewNode runtime producer
    -- If the producer is becoming live because it now has
    -- things consuming it, then we need to tell all producers
    -- it consumes that it is now live.
    --
    -- Since we know that 'producer' is a producer, that means
    -- it isn't an always live consumer (i.e. a watcher).
    when (not $ consumerIsLive node) do
      -- If it is, we add it as a live dependency of all
      -- the producers it depends on.
      ifor_ (node ^. consumerDataLens % #producerNodes)
        \(ProducerId pdep) _ ->
          producerAddLiveConsumer runtime pdep producer
  -- we now add the consumer to this producer's list.
  adjustNode runtime producer $
    producerDataLens % #liveConsumerNodes %~ Set.insert (ConsumerId consumer)

-- | Remove the consumer from being a dependency of the producer.
--
-- This is the same thing as producerRemoveLiveConsumerAtIndex from
-- the signal-polyfill.
--
-- NOTE: I should probably make this throw an error if the consumer
-- we're removing doesn't exist in the producer.
removeConsumerFromProducer ::
  (MonadIO m, IsConsumer c, IsProducer p) =>
  SignalRuntimeState -> RNKey c a -> RNKey p b -> m ()
removeConsumerFromProducer runtime consumer producer = do
  -- NOTE: the polyfill does things in a slightly different order
  -- but I don't think it matters.
  adjustNode runtime producer $
    producerDataLens % #liveConsumerNodes %~ Set.delete (ConsumerId consumer)
  -- If the producer is also a consumer
  for_ (asConsumer producer) \Witness -> do
    node <- viewNode runtime producer
    -- Since we're currently removing a consumer from this producer,
    -- we need to check if it's no longer live, and thus we need to
    -- remove it from all the producers it was consuming.
    when (not $ consumerIsLive node) do
      ifor_ (node ^. consumerDataLens % #producerNodes)
        \(ProducerId pdep) _ ->
          removeConsumerFromProducer runtime producer pdep

-- | Disconnect this consumer from the graph.
--
-- Should not be a producer that is dependend on by
-- other things.
consumerDestroy ::
  (IsConsumer k, MonadIO m) =>
  SignalRuntimeState -> RNKey k a -> m ()
consumerDestroy runtime key = error "TODO"

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
