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
  ComputedSignal,
  ReadableSignal(..),
  WritableSignal(..),
  SignalRuntimeState,
  CanWrite,
  runCanWrite,
  watcherBind, Watcher,
) where

import Data.Kind (Type)
import Control.Monad (void)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Signal.Graph

-- Newtype'd so that we don't leak the reader and monadIO instances.
newtype Reactive a = MkReactive { unReactive :: Reading a }
  deriving (Functor, Applicative, Monad)

-- | I don't know what exactly I should call this, but it's
-- where you can write to signals. Used for IO
--
-- I don't know exactly what escape hatches I'm going to need.
newtype CanWrite a = MkCanWrite
  { unCanWrite :: ReaderT SignalRuntimeState IO a }
  deriving (Functor, Applicative, Monad)

runCanWrite :: SignalRuntimeState -> CanWrite a -> IO a
runCanWrite runtime m = liftIO $ flip runReaderT runtime $ m.unCanWrite

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

watcherBind :: (ReadableSignal k, IsProducer k)
  => SignalRuntimeState -> RNKey k a -> (a -> IO ()) -> IO Watcher
watcherBind runtime dep cb =
  createWatcher runtime dep \watcher -> do
    val <- runReading runtime watcher (readSignal dep).unReactive
    resetWatcher runtime watcher
    cb val
