{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Entities where

import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings] [persistLowerCase|
Person json sql=people
    name String
    UniquePerson name
    deriving Show
|]
