{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module Signal (
  Reactive,
  RwSignal,
  SignalDebug(..),
  ComputedSignal,
  ReadableSignal(..),
  WritableSignal(..),
  SignalRuntimeState,
  CanWrite,
  runCanWrite,
  watcherBind, Watcher,
  createSignalRuntime,
  mkComputedSignal, mkRwSignal,
) where

import Data.Kind (Type)
import Control.Monad (void)

import Debug.Trace qualified as Debug

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Strict qualified as State

import Signal.Graph

-- Newtype'd so that we don't leak the reader and monadIO instances.
newtype Reactive a = MkReactive { asReading :: Reading a }
  deriving (Functor, Applicative, Monad)

instance SignalDebug Reactive where
  logSignalRaw mbShow key = MkReactive $ logSignalRaw mbShow key

-- | I don't know what exactly I should call this, but it's
-- where you can write to signals. Used for IO
--
-- I don't know exactly what escape hatches I'm going to need.
newtype CanWrite a = MkCanWrite
  { unCanWrite :: ReaderT SignalRuntimeState IO a }
  deriving (Functor, Applicative, Monad)

instance SignalDebug CanWrite where
  logSignalRaw mbShow key = MkCanWrite do
    st <- ask
    logSignalState mbShow st key

runCanWrite :: SignalRuntimeState -> CanWrite a -> IO a
runCanWrite runtime m =
  liftIO $ flip runReaderT runtime $ m.unCanWrite

-- I think I'll have an effect type for synchronizing with outside
-- stuff.

-- NOTE: I should probably add something saying that every readable
-- signal is a producer.

class ReadableSignal (k :: ReactiveNodeKind) where
  readSignal :: RNKey k a -> Reactive a

class WritableSignal (k :: ReactiveNodeKind) where
  modifySignal :: RNKey k a -> (a -> a) -> CanWrite ()

  writeSignal :: RNKey k a -> a -> CanWrite ()
  writeSignal signal val = modifySignal signal $ const val

instance ReadableSignal 'RwSignalKind where
  readSignal signal = MkReactive $ readRwSignal signal

instance ReadableSignal 'ComputedSignalKind where
  readSignal signal = MkReactive $ readComputedSignal signal

instance WritableSignal 'RwSignalKind where
  modifySignal signal f = MkCanWrite do
    runtime <- ask
    modifyRwSignal runtime signal f

-- These are unfortunately necessary to properly hide 'Reactive.asReading'.
mkComputedSignal :: (Eq a, MonadIO m) => SignalRuntimeState -> Reactive a -> m (ComputedSignal a)
mkComputedSignal runtime r = createComputedSignal runtime r.asReading

mkRwSignal :: (Eq a, MonadIO m) => SignalRuntimeState -> a -> m (RwSignal a)
mkRwSignal runtime initial = createRwSignal runtime initial

watcherBind :: forall k a. (Show a, ReadableSignal k, IsProducer k)
  => SignalRuntimeState -> RNKey k a -> (a -> IO ()) -> IO (a, Watcher)
watcherBind runtime dep cb = do
  let log :: forall b q m. (MonadIO m, Show b) => RNKey q b -> m ()
      log = logSignalState (Just show) runtime
  watcher <- createWatcher runtime dep \watcher -> do
    log watcher
    log dep
    val <- runReading runtime watcher (readSignal dep).asReading
    resetWatcher runtime watcher
    --Debug.traceM $ "after change " <> show watcher
    --log runtime watcher
    cb val
  -- We call this to make sure that 'dep' is connected to the producers
  -- it depends on.
  val <- runReading runtime watcher (readSignal dep).asReading
  log dep
  pure (val, watcher)
