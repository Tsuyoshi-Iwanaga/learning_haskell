-- エンコーダー
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

-- デコーダー
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset = if even n
      then fromEnum c + halfN
      else fromEnum c + halfN + 1
    rotation = offset `mod` n

-- 4文字のアルファベット
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder vals = map rot4ldecoder vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4ldecoder = rotNdecoder alphaSize

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L3, L4, L1, L1, L2]

-- 実行
-- fourLetterDecoder (fourLetterEncoder fourLetterMessage)

-- 文字列
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotCharDecoder = rotNdecoder alphaSize

-- 実行
-- rotDecoder (rotEncoder "Jean-Paul likes Simone")
