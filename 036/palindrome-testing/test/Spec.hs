import Lib
import Data.Char (isPunctuation)
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Text as T

-- プロパティ(性質のテスト)ここでは句読点のあるなしのtextをpreprocessを通すと同じになるか
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = (isPalindrome (T.reverse text)) == (isPalindrome text)

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
  then putStrLn passStatement
  else putStrLn failStatement

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 200 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  putStrLn "done!"