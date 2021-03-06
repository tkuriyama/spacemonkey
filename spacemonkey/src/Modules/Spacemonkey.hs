
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
  :<|> "grid" :> Capture "wid" WorldId :> Get '[JSON] [Cell]
  :<|> "msgs" :> Capture "wid" WorldId :>
       Capture "recentN" Int :> Get '[JSON] [Message]
  :<|> "user" :> Capture "uid" UserId :>
       Get '[JSON] (Maybe User)
  :<|> "users" :> Capture "wid" WorldId :> Get '[JSON] [User]
  :<|> "move" :> Capture "uid" UserId :>
       Capture "direction" Direction:> Put '[JSON] Direction
  :<|> "reface" :> Capture "uid" UserId :>
       Capture "direction" Direction:> Put '[JSON] Direction
  :<|> "cellColor" :> Capture "wid" WorldId :>
       Capture "x" Int :> Capture "y" Int :> Capture "color" Color :>
       Put '[JSON] Color
  :<|> "cellCType" :> Capture "wid" WorldId :>
       Capture "x" Int :> Capture "y" Int :> Capture "celltype" CellType :>
       Put '[JSON] CellType
  :<|> "clearCell" :> Capture "wid" WorldId :> Capture "x" Int :>
       Capture "y" Int :> Put '[JSON] Bool
  :<|> "cellValue" :> Capture "wid" WorldId :> Capture "x" Int :>
       Capture "y" Int :> ReqBody '[JSON] T.Text :> Put '[JSON] T.Text
