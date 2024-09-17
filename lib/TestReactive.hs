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

-- | Takes a function that sets something on the screen,
-- and returns a function to call when a button is pressed.
setupReactive :: (String -> IO ()) -> IO (IO ())
setupReactive setOutput = do
  setOutput "FIRST"
  runtime <- createSignalRuntime
  counter <- createRwSignal runtime (0::Int)
  asString <- createComputedSignal runtime do
    c <- readRwSignal counter
    pure $ show c
  void $ watcherBind runtime asString \c -> setOutput c
  pure do
    modifyRwSignal runtime counter (+1)
