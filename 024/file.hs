import System.IO

main :: IO ()
main = do
  -- 読み込み
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  secondLine <- hGetLine helloFile
  putStrLn firstLine
  putStrLn secondLine
  -- 書き込み
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile firstLine
  hPutStrLn goodbyeFile secondLine
  -- ファイルを閉じる
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"