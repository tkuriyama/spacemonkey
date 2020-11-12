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
   Cell
     env WorldId
     x Int
     y Int
     UniqueCell env x y
     color Color
     value T.Text
     cType CellType
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
     loc CellId
     deriving Show Generic
  |]


-- TH invocation for servant-elm
deriveBoth defaultOptions ''World
deriveBoth defaultOptions ''Cell
deriveBoth defaultOptions ''Message
deriveBoth defaultOptions ''User

-- Servant API definitions
type API =
       "getWorldId" :> Capture "env" Environment :>
       Get '[JSON] (Maybe WorldId)
  :<|> "getWorld" :> Capture "wid" WorldId :>
       Get '[JSON] (Maybe World)
  :<|> "getCell" :> Capture "worldid" WorldId :> Get '[JSON] [Cell]
  :<|> "getMsgs" :> Capture "worldid" WorldId :>
       Capture "recentN" Int :> Get '[JSON] [Message]
  :<|> "getUsers" :> Capture "worldid" WorldId :> Get '[JSON] [User]
  :<|> "setCellColor" :> Capture "worldid" WorldId :>
       Capture "x" Int :> Capture "y" Int :> Capture "c" Color :>
       Put '[JSON] Color
