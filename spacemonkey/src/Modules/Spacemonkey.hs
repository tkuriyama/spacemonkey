
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
     env Env
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
     senderId UserId
     value T.Text
     deriving Show Generic
   User
     env WorldId
     name T.Text
     UniqueUser env name
     x Int
     y Int
     facing Direction
     deriving Show Generic
  |]


-- TH invocation for servant-elm
deriveBoth defaultOptions ''World
deriveBoth defaultOptions ''Cell
deriveBoth defaultOptions ''Message
deriveBoth defaultOptions ''User

-- Servant API definitions
-- For use with Capture, sum types from SpacemonkeyEnum should be
-- fully spelled-out in lower case (dependency in PostCodeGen.hs)

type API =
       "worldId" :> Capture "env" Env :>
       Get '[JSON] (Maybe WorldId)
  :<|> "world" :> Capture "wid" WorldId :>
       Get '[JSON] (Maybe World)
  :<|> "grid" :> Capture "worldid" WorldId :> Get '[JSON] [Cell]
  :<|> "msgs" :> Capture "worldid" WorldId :>
       Capture "recentN" Int :> Get '[JSON] [Message]
  :<|> "user" :> Capture "userid" UserId :>
       Get '[JSON] (Maybe User)
  :<|> "users" :> Capture "worldid" WorldId :> Get '[JSON] [User]
  :<|> "cellColor" :> Capture "worldid" WorldId :>
       Capture "x" Int :> Capture "y" Int :> Capture "color" Color :>
       Put '[JSON] Color
  :<|> "move" :> Capture "userId" UserId :>
       Capture "direction" Direction:> Put '[JSON] Direction
  :<|> "reface" :> Capture "userId" UserId :>
       Capture "direction" Direction:> Put '[JSON] Direction
