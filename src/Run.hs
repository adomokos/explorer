{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Run
  -- ( run
  -- )
           where

import Import

import qualified DB
import qualified GitHubProxy as GP
import Util (showEither)

run :: RIO App ()
run =
  DB.run
