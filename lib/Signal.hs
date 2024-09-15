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
  ReadableSignal(..),
  WritableSignal(..),
  SignalRuntimeState,
  CanWrite,
  runCanWrite,
) where

import Data.Kind (Type)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Signal.Graph

newtype Reactive a = MkReactive { unReactive :: Reading a }

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

class ReadableSignal (f :: Type -> Type) where
  readSignal :: f a -> Reactive a

class WritableSignal (f :: Type -> Type) where
  modifySignal :: f a -> (a -> a) -> CanWrite ()

  writeSignal :: WritableSignal f => f a -> a -> CanWrite ()
  writeSignal signal val = modifySignal signal $ const val

instance ReadableSignal RwSignal where
  readSignal signal = MkReactive $ readRwSignal signal

instance ReadableSignal ComputedSignal where
  readSignal signal = MkReactive $ readComputedSignal signal

instance WritableSignal RwSignal where
  modifySignal signal f = MkCanWrite do
    runtime <- ask
    modifyRwSignal runtime signal f
