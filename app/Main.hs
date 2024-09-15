module Main (myMain, main) where

import GHC.Wasm.Prim

import Lib

foreign import javascript unsafe "console.log($1)"
  jsLog :: JSString -> IO ()
foreign import javascript unsafe "document.body"
  getBody :: IO JSVal
foreign import javascript unsafe "$1.innerText = $2"
  setInnerText :: JSVal -> JSString -> IO ()
foreign export ccall myMain :: IO ()

myMain :: IO ()
myMain = do
  jsLog (toJSString "thing9")
  bindRoot "#root" $ HtmlElement Div [] [HtmlText "test"]
  --body <- getBody
  -- setInnerText body $ toJSString "hello world"

main = error "not used"
