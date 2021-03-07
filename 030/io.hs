askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn
-- helloName = askForName >> getLine >>= putStrLn.nameStatement
-- helloName = askForName >> getLine >>= return.nameStatement >>= putStrLn
