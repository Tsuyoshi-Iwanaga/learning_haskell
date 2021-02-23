{-# LANGUAGE OverloadedStrings #-} -- LANGUAGEプラグマ
import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "password"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

sampleInput :: T.Text
sampleInput = "this\nis for\ninput"

-- T.lines sampleInput
-- T.words sampleInput
-- T.unlines (T.lines sampleInput)
-- T.unwords (T.words sampleInput)

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- T.splitOn breakText exampleText
-- T.intercalate breakText (T.splitOn breakText exampleText)

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"