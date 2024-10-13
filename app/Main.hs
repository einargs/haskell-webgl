{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
module Main (myMain, main) where

import GHC.Wasm.Prim

import Lib
import Debug.Trace qualified as Debug

foreign import javascript unsafe "console.log($1)"
  jsLog :: JSString -> IO ()
foreign import javascript unsafe "document.body"
  getBody :: IO JSVal

-- If you just export IO(), it only runs once.
foreign import javascript "wrapper"
  jsMakeCallback :: (JSVal -> IO ()) -> IO JSVal
-- it's safe because we can need to call back into haskell
foreign import javascript safe "document.querySelector('#but').addEventListener('click', () => $1())"
  jsOnClick :: JSVal -> IO ()
foreign import javascript unsafe "document.querySelector('#out').innerText = $1"
  jsSetOut :: JSString -> IO ()
foreign export ccall myMain :: IO ()

clog :: String -> IO ()
clog = jsLog . toJSString

makeCallback :: IO () -> IO JSVal
makeCallback act = jsMakeCallback \_ -> act

primaryView :: View
primaryView = EmbededComponent do
  count1 <- rwSignal (0::Int)
  count2 <- rwSignal (0::Int)
  Debug.traceM $ "count1 signal key: " <> show count1
  Debug.traceM $ "count2 signal key: " <> show count2
  sum <- computedSignal $ liftA2 (+) (readSignal count1) (readSignal count2)
  asString <- computedSignal $ show <$> readSignal sum
  Debug.traceM $ "asString signal key: " <> show asString
  let mkBut signal = RawHtml Button [] ["click" #=> onClick signal] [RawText "inc"]
      out1 = RawHtml Div [] [] [EmbededReactive $ RawText . show <$> readSignal sum]
      out2 = RawHtml Div [] [] [EmbededReactive $ RawText <$> readSignal asString]
      onClick signal = do
        modifySignal signal \i -> Debug.trace (show signal <> " clicked " <> show i) $ i+1
  pure $ RawHtml Div [] [] [mkBut count1, mkBut count2, out1, out2]

myMain :: IO ()
myMain = do
  jsLog (toJSString "thing0")
  bindRoot "#root" primaryView
{-
myMain :: IO ()
myMain = do
  jsLog (toJSString "thing9")
  bindRoot "#root" $ HtmlElement Div [] [HtmlText "test"]
  let setOut = jsSetOut . toJSString
  val <- makeCallback $ do 
    clog "HEY"
    setOut "CLICKED"
    pure ()
  cb <- setupReactive \s -> do
    putStrLn s
    setOut s
  --setOut "HEY"
  jsCb <- makeCallback cb
  jsOnClick val
  putStrLn "WHY"
  jsOnClick jsCb
  --cb
  --cb
  --body <- getBody
  -- setInnerText body $ toJSString "hello world"
  pure ()
-}

main :: IO ()
main = error "not used"
