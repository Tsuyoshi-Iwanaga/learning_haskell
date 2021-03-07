askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello " ++ name ++ "!"

-- Monadを使って記述
helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

-- do表記
helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

echo :: IO ()
echo = getLine >>= putStrLn

echo2 :: IO ()
echo2 = do
  message <- getLine
  putStrLn message