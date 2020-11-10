{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Modules.Spacemonkey where

import           Data.Text as T

import           GHC.Generics
import           Elm.Derive   (defaultOptions, deriveBoth)
import           Servant.API

import           Database.Persist.TH

import           Helpers.SpacemonkeyEnum

-- ------------------------------------------------------


-- Data definitions in TH invocation format for Persistent
share
  [mkPersist sqlSettings , mkMigrate "migrateAll"]
  [persistLowerCase|
   World
     env Environment
     UniqueEnv env
     maxX Int
     maxY Int
     deriving Show Generic
   Grid
     env WorldId
     x Int
     y Int
     UniqueCell env x y
     cellColor Color default White
     cellType CellType default Std
     cellValue T.Text default ""
     cellUser UserId Maybe
     deriving Show Generic
   Message
     env WorldId
     -- timestamp Int
     -- sender UserId
     value T.Text
     deriving Show Generic
   User
     env WorldId
     name T.Text
     UniqueUser env name
     loc GridId
     deriving Show Generic
  |]


-- TH invocation for servant-elm
deriveBoth defaultOptions ''World
deriveBoth defaultOptions ''Grid
deriveBoth defaultOptions ''Message
deriveBoth defaultOptions ''User


-- Servant API definitions
type API =
       "getWorld" :> Capture "env" Environment :>
       Get '[JSON] (Maybe (WorldId, World))
  :<|> "getGrid" :> Capture "worldid" WorldId :> Get '[JSON] [Grid]
  :<|> "getMsgs" :> Capture "worldid" WorldId :>
       Capture "recentN" Int :> Get '[JSON] [Message]
  :<|> "getUsers" :> Capture "worldid" WorldId :> Get '[JSON] [User]
  :<|> "setGridColor" :> Capture "worldid" WorldId :>
       Capture "x" Int :> Capture "y" Int :> Capture "c" Color :>
       Put '[JSON] Color
