{-# LANGUAGE OverloadedStrings #-} -- LANGUAGEプラグマ
import System.Environment
import System.Random
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  args <- getArgs --ファイル名を取得
  let filename = head args
  imageFile <- BC.readFile filename -- ファイルを読み込み
  -- glitched <- randomReplaceByte imageFile
  -- glitched <- randomSortSection imageFile
  -- foldMを使ってI/Oアクションを連結する
  glitched <- foldM (\bytes func -> func bytes) imageFile
    [randomReplaceByte
    ,randomSortSection
    ,randomReplaceByte
    ,randomSortSection
    ,randomReplaceByte]
  let glitchedFileName = mconcat ["glitched_", filename]
  BC.writeFile glitchedFileName glitched -- 新しいファイルに書き込む
  print "all done!"

-- Charの範囲が0から255まで(ASKII)になるように剰余をとる
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 256

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- バイトを置き換える
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

-- 乱数の生成
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)


-- 方法2: ランダムなバイトを置き換える
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)
