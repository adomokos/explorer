module Explorer.Util
  ( plus2
  , showEither
  , showAndReturnEither
  , toEither
  , fetch2Data
  , fetch3Data
  )
where

import Explorer.Import

plus2 :: Int -> Int
plus2 = (+ 2)

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Left: " <> displayShow err
  Right value -> "Right: " <> displayShow value

showAndReturnEither :: (Show a, Show b) => Either a b -> RIO App (Either a b)
showAndReturnEither value  = do
  logInfo $ showEither value
  pure value

toEither :: a -> Maybe b -> Either a b
toEither appError = maybe (Left appError) Right

fetch2Data :: MonadUnliftIO m => m a -> m b -> m (a, b)
fetch2Data action1 action2 =
  runConcurrently $ (,)
    <$> Concurrently action1
    <*> Concurrently action2

fetch3Data :: MonadUnliftIO m => m a -> m b -> m c -> m (a, b, c)
fetch3Data action1 action2 action3 =
  runConcurrently $ (,,)
    <$> Concurrently action1
    <*> Concurrently action2
    <*> Concurrently action3
