{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module View (
  TagKind (..),
  View (..),
  ViewAttr(..),
  AttrName(..),
  AttrValue(..),
  renderAttrValue,
  elementTagName,
  EventName,
  domEventName,
  EventListener(..),
  (#=>)
) where

import Component
import Signal

data AttrValue
  = AttrString String
  | AttrInt Int
  deriving (Eq)

renderAttrValue :: AttrValue -> String
renderAttrValue (AttrString s) = show s
renderAttrValue (AttrInt i) = show i

instance Show AttrValue where
  show = renderAttrValue

newtype AttrName = AttrName String
  deriving (Show, Eq)

data TagKind
  = Div
  | Button
  deriving Eq

instance Show TagKind where
  show = elementTagName

elementTagName :: TagKind -> String
elementTagName tag = case tag of
  Div -> "div"
  Button -> "button"

data ViewAttr
  = StaticAttr AttrName AttrValue
  | DynAttr AttrName (Reactive AttrValue)

instance Show ViewAttr where
  show (StaticAttr (AttrName name) value) = name <> ": " <> show value
  show (DynAttr (AttrName name) _) = name <> ": dynamic"

instance Eq ViewAttr where
  (StaticAttr n1 v1) == (StaticAttr n2 v2) = n1==n2 && v1==v2
  _ == _ = False

newtype EventName = EventName String

domEventName :: EventName -> String
domEventName (EventName str) = str

instance Show EventName where
  show = domEventName

-- NOTE: I'll probably make this a GADT that links the name of the
-- watched event to the type of the event the callback gets via
-- 'EventName a' and 'Event a'. But for now no need. Maybe I'll
-- use the type literal strings.
data EventListener = EventListener EventName (CanWrite ())

(#=>) :: String -> CanWrite () -> EventListener
str #=> cb = EventListener (EventName str) cb

data View
  = RawHtml TagKind [ViewAttr] [EventListener] [View]
  | ViewFragment [View]
  | RawText String
  | EmbededComponent (Component View)
  | EmbededReactive (Reactive View)

instance Eq View where
  (RawHtml t1 a1 [] v1) == (RawHtml t2 a2 [] v2) =
    t1==t2 && a1 == a2 && v1==v2
  (ViewFragment v1) == (ViewFragment v2) = v1 == v2
  (RawText s1) == (RawText s2) = s1 == s2
  _ == _ = False

instance Show View where
  show = \case
    RawHtml tag attrs listeners views ->
      show tag <> " " <>
      show attrs <> " " <>
      show (length listeners) <> " listeners " <>
      show views
    ViewFragment views -> "fragment " <> show views
    RawText str -> str
    EmbededComponent _ -> "component view"
    EmbededReactive _ -> "reactive view"
