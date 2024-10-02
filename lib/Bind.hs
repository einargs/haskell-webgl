{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | This module is in charge of rendering things.
module Bind (
  bindRoot
) where

import Control.Monad.Reader
  ( MonadReader
  , ReaderT
  , runReaderT
  , ask
  )
import Control.Monad.IO.Class
import Control.Monad (void)
import Data.IORef qualified as IORef

import Data.Foldable (for_)

import GHC.Wasm.Prim
import Data.Coerce (Coercible, coerce)
import Debug.Trace qualified as Debug

import View
import Component

import Signal
import Component (runComponent)
import qualified Data.IORef as IORef
import Signal.Graph (logSignalState)
--import Signal.Graph (createComputedSignal)

-- NOTE: try using DocumentFragment when composing things.
--
-- Wait, what does it even offer? If I've rendered everything
-- to

foreign import javascript unsafe "document.createDocumentFragment()"
  jsCreateDocumentFragment :: IO JSNode

foreign import javascript unsafe "document.querySelector($1)"
  jsQuerySelector :: JSString -> IO JSNode

foreign import javascript unsafe "document.createElement($1)"
  jsCreateElement :: JSString -> IO JSNode

foreign import javascript unsafe "console.log($1)"
  jsLog :: JSNode -> IO ()

foreign import javascript unsafe "new Text($1)"
  jsCreateTextNode :: JSString -> IO JSNode

foreign import javascript unsafe "$1.attributes[$2] = $3"
  jsSetNodeAttr :: JSNode -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.append($2)"
  jsNodeAppend :: JSNode -> JSNode -> IO ()

foreign import javascript unsafe "$1.remove()"
  jsNodeRemoveSelf :: JSNode -> IO ()

foreign import javascript unsafe "$1.replaceWith(...$2)"
  jsNodeReplaceSelf :: JSNode -> JSArray -> IO ()

foreign import javascript unsafe "$1.push($2)"
  jsArrayPush :: JSArray -> JSVal -> IO ()

foreign import javascript unsafe "[]"
  jsNewArray :: IO JSArray

foreign import javascript "wrapper"
  jsMakeCallback :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript safe "$1.addEventListener($2, $3)"
  jsAddEventListener :: JSNode -> JSString -> JSVal -> IO ()

-- TODO: try removing the extra redundant argument.
makeCallback :: IO () -> IO JSVal
makeCallback act = jsMakeCallback \_ -> act

newtype JSNode = JSNode {unwrap::JSVal}
newtype JSArray = JSArray {unwrap::JSVal}

listToJSArray :: (MonadIO m, Coercible a JSVal) => [a] -> m JSArray
listToJSArray xs = do
  newArray <- liftIO jsNewArray
  f newArray xs
  where
  f a [] = pure a
  f a (x:xs) = do
    liftIO $ jsArrayPush a (coerce x)
    f a xs

data BinderState = BinderState
  { signalRuntime :: !SignalRuntimeState
  }

newtype Binder a = MkBinder { unBinder :: ReaderT BinderState IO a }
  deriving (Functor, Applicative, Monad, MonadReader BinderState, MonadIO)



-- TODO: how do I track which elements to remove that were
-- added by a document fragment?

-- How do I re-render stuff? How do I remove old stuff when things
-- change?

nodeReplaceSelf :: JSNode -> [JSNode] -> Binder ()
nodeReplaceSelf node children = do
  jsArray <- listToJSArray children
  liftIO $ jsNodeReplaceSelf node jsArray

setNodeAttr :: JSNode -> AttrName -> AttrValue -> Binder ()
setNodeAttr node (AttrName name) value =
  liftIO $ jsSetNodeAttr node name' value'
  where
  value' = toJSString $ renderAttrValue value
  name' = toJSString name

-- | Bind a reactive value to a callback.
--
-- Only invoke the callback for future values; the current
-- value will not be used.
bindReactiveAfter :: (Show a, Eq a) => Reactive a -> (a -> Binder ()) -> Binder a
bindReactiveAfter reactive cb = do
  st <- ask
  let runtime = st.signalRuntime
  computed <- mkComputedSignal runtime reactive
  (val, _watcher) <- liftIO $ watcherBind runtime computed $
    runBinder st . cb
  pure val

-- | Bind a reactive value to a callback and immediately invoke
-- the callback with the current value of the reactive
-- computation.
bindReactiveImm :: (Show a, Eq a) => Reactive a -> (a -> Binder ()) -> Binder a
bindReactiveImm r cb = do
  val <- bindReactiveAfter r cb
  cb val
  pure val

bindAttr :: JSNode -> ViewAttr -> Binder ()
bindAttr node (StaticAttr name value) = setNodeAttr node name value
bindAttr node (DynAttr name reactive) =
  void $ bindReactiveImm reactive $ setNodeAttr node name

addEventListener :: JSNode -> EventListener -> Binder ()
addEventListener node (EventListener name update) = do
  st <- ask
  let jsName = toJSString $ domEventName name
  jsCb <- liftIO $ makeCallback $ runCanWrite st.signalRuntime update
  liftIO $ jsAddEventListener node jsName jsCb

renderElement :: View -> Binder JSNode
renderElement (RawHtml tag attrs events children) = do
  elem <- liftIO $ jsCreateElement $ toJSString $ elementTagName tag
  for_ attrs $ bindAttr elem
  for_ events $ addEventListener elem
  for_ children \child -> do
    childElem <- renderElement child
    liftIO $ jsNodeAppend elem childElem
  pure elem
renderElement (ViewFragment views) = error "TODO"
renderElement (EmbededReactive r) = do
  ref <- liftIO $ IORef.newIORef Nothing
  val <- bindReactiveAfter r \view -> do
    old <- liftIO $ IORef.readIORef ref
    case old of
      Nothing -> error "update callback was called before first node could be rendered"
      Just oldNode -> do
        newNode <- renderElement view
        liftIO $ IORef.writeIORef ref $ Just newNode
        nodeReplaceSelf oldNode [newNode]
  firstNode <- renderElement val
  liftIO $ IORef.writeIORef ref $ Just firstNode
  pure firstNode
renderElement (EmbededComponent c) = do
  runtime <- (.signalRuntime) <$> ask 
  view <- liftIO $ runComponent runtime c
  renderElement view
renderElement (RawText str) =
  liftIO $ jsCreateTextNode $ toJSString str

runBinder :: BinderState -> Binder a -> IO a
runBinder st m = flip runReaderT st m.unBinder

bindRoot :: String -> View -> IO ()
bindRoot rootQuery view = do
  runtime <- createSignalRuntime
  putStrLn rootQuery

  let st = BinderState {signalRuntime=runtime}
  rootElem <- jsQuerySelector $ toJSString rootQuery
  liftIO $ jsLog rootElem
  node <- runBinder st $ renderElement view
  jsNodeAppend rootElem node
