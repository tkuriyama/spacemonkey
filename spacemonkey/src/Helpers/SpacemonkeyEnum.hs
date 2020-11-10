{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

-- Sum Types for Persistent need to be in separate module
-- https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md

module Helpers.SpacemonkeyEnum where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           GHC.Generics
import           Elm.Derive   (defaultOptions, deriveBoth)
import           Web.HttpApiData

import           Database.Persist.TH

data Environment
  = Dev
  | Prod
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance FromHttpApiData Environment where
  parseUrlPiece :: T.Text -> Either T.Text Environment
  parseUrlPiece = myParse

data Color
  = White
  | Yellow
  | Red
  | Green
  | Blue
  | Grey
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance FromHttpApiData Color where
  parseUrlPiece :: T.Text -> Either T.Text Color
  parseUrlPiece = myParse

data CellType
    = Std
    | Link
    | Text
    | Fixed
    deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance FromHttpApiData CellType where
  parseUrlPiece :: T.Text -> Either T.Text CellType
  parseUrlPiece = myParse


myParse :: (Show a, Enum a, Bounded a) => T.Text -> Either T.Text a
myParse s = maybe (Left "cannot find color") Right $ M.lookup s m
  where m = M.fromList $ map (\v -> (T.pack $ show v, v)) [minBound..maxBound]


-- TH invocations for persisten
derivePersistField "Environment"
derivePersistField "Color"
derivePersistField "CellType"

-- TH invocation for servant-elm
deriveBoth defaultOptions ''Environment
deriveBoth defaultOptions ''Color
deriveBoth defaultOptions ''CellType
