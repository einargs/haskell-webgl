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
  runComponent
) where

import Control.Monad.State.Strict
  ( MonadState (..),
    StateT,
    modify',
    runStateT,
  )

import Signal

data ComponentState = ComponentState
  { signalRuntime :: !SignalRuntimeState
  }

newtype Component a = MkComponent { unComponent :: StateT ComponentState IO a }
  deriving (Functor, Applicative, Monad)
-- This uses StateT in case I want to carry some other state.

runComponent :: SignalRuntimeState -> Component a -> IO a
runComponent runtime m = fst <$> flip runStateT st m.unComponent
  where st = ComponentState {signalRuntime=runtime}
