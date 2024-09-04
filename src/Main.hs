module Main (myMain, main) where
import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()
foreign export ccall myMain :: IO ()

myMain :: IO ()
myMain = do
  js_log (toJSString "hey")

main = error "not used"
