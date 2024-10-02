{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module Component (
  Component,
  runComponent,
  computedSignal,
  rwSignal,
) where

import Control.Monad.Reader
  ( MonadReader
  , ReaderT
  , runReaderT
  , ask
  )
import Control.Monad.Trans.Class (lift)

import Signal

data ComponentState = ComponentState
  { signalRuntime :: !SignalRuntimeState
  }

newtype Component a = MkComponent { unComponent :: ReaderT ComponentState IO a }
  deriving (Functor, Applicative, Monad)

runComponent :: SignalRuntimeState -> Component a -> IO a
runComponent runtime m = flip runReaderT st m.unComponent
  where st = ComponentState {signalRuntime=runtime}

getSignalRuntime :: Component SignalRuntimeState
getSignalRuntime = MkComponent $ (.signalRuntime) <$> ask

-- | Create a computed signal.
computedSignal :: (Eq a) => Reactive a -> Component (ComputedSignal a)
computedSignal r = do
  runtime <- getSignalRuntime
  MkComponent $ lift $ mkComputedSignal runtime r

-- | Create a read-write signal.
rwSignal :: (Eq a) => a -> Component (RwSignal a)
rwSignal def = do
  runtime <- getSignalRuntime
  MkComponent $ lift $ mkRwSignal runtime def

