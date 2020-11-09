{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes                #-}

module Modules.HelloServerPersist where

import           GHC.Generics
import           Elm.Derive   (defaultOptions, deriveBoth)
import           Servant.API

import           Database.Persist.TH

-- ------------------------------------------------------
-- -- The Haskell structure that we want to encapsulate

share
  [mkPersist sqlSettings , mkMigrate "migrateAll"]
  [persistLowerCase|
   ServerState
    state String
    counter Int
    deriving Eq Show Generic
  |]

-- TH invocation for servant-elm
deriveBoth defaultOptions ''ServerState

type ServantAPI =
       "request" :> Get '[JSON] (Maybe ServerState)
  :<|> "increment" :> Put '[JSON] (Maybe ServerState)

