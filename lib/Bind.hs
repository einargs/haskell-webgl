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

import Data.Foldable (for_)

import GHC.Wasm.Prim

import View

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

foreign import javascript unsafe "new Text($1)"
  jsCreateTextNode :: JSString -> IO JSNode

foreign import javascript unsafe "$1.append($2)"
  jsAppend :: JSNode -> JSNode -> IO ()

newtype JSNode = JSNode {unwrap::JSVal}

elementTagName :: TagKind -> JSString
elementTagName elem = toJSString $ case elem of
  Div -> "div"

renderElement :: HtmlElement -> IO JSNode
renderElement (HtmlElement tag attrs children) = do
  elem <- jsCreateElement $ elementTagName tag
  -- TODO: render the attributes
  for_ children \child -> do
    childElem <- renderElement child
    jsAppend elem childElem
  pure elem
renderElement (HtmlText str) =
  jsCreateTextNode $ toJSString str

--renderFragment :: Html -> IO JSDocumentFragment
--renderFragment (Html tag attrs children) = error "TODO"

-- How do I re-render stuff? How do I remove old stuff when things
-- change?

bindRoot :: String -> HtmlElement -> IO ()
bindRoot rootQuery html = do
  rootElem <- jsQuerySelector $ toJSString rootQuery
  node <- renderElement html
  jsAppend rootElem node

