module Explorer.Util
  ( applyFirst
  , showEither
  , showAndReturnEither
  , toEither
  , fetch2Data
  , fetch3Data
  )
where

import RIO
-- import Explorer.Import

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Left: " <> displayShow err
  Right value -> "Right: " <> displayShow value

showAndReturnEither ::
  ( MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , Show a
  , Show b)
  => Either a b
  -> m (Either a b)
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

applyFirst :: (a -> a) -> [a] -> [a]
-- This could work, ut it's not simple, it's too clever
-- applyFirst f xs = (maybe [] (\x -> [f x]) $ listToMaybe xs) ++ drop 1 xs
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x:xs) = f x : xs
