{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , showEither
  )
where

import RIO

plus2 :: Int -> Int
plus2 = (+ 2)

showEither :: (Show a, Show b) => Either a b -> Utf8Builder
showEither eValue = case eValue of
  Left  err   -> "Error: " <> displayShow err
  Right value -> displayShow value

