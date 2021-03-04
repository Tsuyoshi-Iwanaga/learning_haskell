-- Fanctor
-- fmap :: Fanctor f => (a -> b) -> f a -> f b
-- つまりコンテキストに属さない一般的な関数を受け取り、同じコンテナやコンテキストに属したverの関数を返す
-- <$>はfmapのシノニム

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- incMaybe :: Maybe Int -> Maybe Int
-- incMaybe (Just n) = Just (n + 1)
-- incMaybe Nothing = Nothing

-- FanctorのインスタンスであるMaybeの定義
-- instance Functor Maybe where
--   fmap func (Just n) = Just (func n)
--   fmap Nothing = Nothing

-- fmapを使う
-- fmap (+ 1) successfulRequest
-- fmap (+ 1) failedRequest

--　一般的には<$>(fmapのシノニム)を使う
-- (+ 1) <$> successfulRequest
-- (+ 1) <$> failedRequest

-- Maybeコンテキストは保ちつつ、showで数値を文字列に変換する
successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest