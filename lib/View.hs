{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module View (TagKind(..), View(..), HtmlElement(..)) where

import Signal
import Component

data AttrValue = AttrValue
newtype AttrName = AttrName String

data TagKind
  = Div

data HtmlAttr = HtmlAttr String
data HtmlElement
  = HtmlElement TagKind [HtmlAttr] [HtmlElement]
  | HtmlText String

data Attribute
  = StaticAttr AttrName AttrValue
  | DynAttr AttrName (Reactive AttrValue)

data ViewAttr = ViewAttr

data View
  = RawHtml TagKind [ViewAttr] [View]
  | Fragment [View]
  | EmbededComponent (Component View)
  | EmbededReactive (Reactive View)

renderView :: View -> IO HtmlElement
renderView = error "TODO"
