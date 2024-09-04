module Main (myMain, main) where

foreign import javascript unsafe "console.log('test ' + $1)"
  js_test :: Int -> IO ()
foreign export ccall myMain :: IO ()
myMain :: IO ()
myMain = do
  js_test 10

main :: IO ()
main = js_test 10
