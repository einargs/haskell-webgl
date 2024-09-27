{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module TestReactive (setupReactive) where

import Control.Monad (void)

import Signal.Graph
import Signal
import Debug.Trace qualified as Debug

-- | Takes a function that sets something on the screen,
-- and returns a function to call when a button is pressed.
setupReactive :: (String -> IO ()) -> IO (IO ())
setupReactive setOutput = do
  setOutput "FIRST1"
  runtime <- createSignalRuntime
  counter <- createRwSignal runtime (0::Int)
  logSignalState runtime counter
  Debug.traceM $ "created counter " <> show counter
  asString <- createComputedSignal runtime do
    Debug.traceM $ "counter read in computed " <> show counter
    logSignalState runtime counter
    c <- readRwSignal counter
    pure $ show c
  void $ watcherBind runtime asString $ setOutput
  pure do
    modifyRwSignal runtime counter (+1)
    logSignalState runtime counter
