import System.Environment
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

-- (countsText . getCounts) "this is\n some text"

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])

-- main = do
--   args <- getArgs
--   let fileName = head args
--   input <- readFile fileName
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--   putStrLn summary