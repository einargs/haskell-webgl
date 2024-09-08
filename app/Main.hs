module Main (myMain, main) where

import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()
foreign import javascript unsafe "document.body"
  getBody :: IO JSVal
foreign import javascript unsafe "$1.innerText = $2"
  setInnerText :: JSVal -> JSString -> IO ()
foreign export ccall myMain :: IO ()

myMain :: IO ()
myMain = do
  js_log (toJSString "thing7")
  --body <- getBody
  -- setInnerText body $ toJSString "hello world"

main = error "not used"
