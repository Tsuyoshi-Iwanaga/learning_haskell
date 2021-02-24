import System.IO

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  goodbyeFile <- openFile "goodbye.txt" WriteMode

  firstLine <- hGetLine helloFile
  secondLine <- hGetLine helloFile

  putStrLn firstLine
  putStrLn secondLine

  hPutStrLn goodbyeFile firstLine
  hPutStrLn goodbyeFile secondLine

  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"