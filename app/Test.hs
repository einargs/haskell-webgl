module Test where
foreign import javascript unsafe "console.log('hey')"
  hey:: IO ()


thing :: IO ()
thing = do
  hey
  hey
