{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Run
  -- ( run
  -- )
           where

import Import

import qualified DB
import qualified GitHubProxy as GP

run :: RIO App ()
run = do
  logInfo "Hello"
  -- GP.run
  DB.run
