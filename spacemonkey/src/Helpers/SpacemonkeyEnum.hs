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

data Env
  = Dev
  | Prod
  deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance FromHttpApiData Env where
  parseUrlPiece :: T.Text -> Either T.Text Env
  parseUrlPiece = myParse

instance ToHttpApiData Env where
  toUrlPiece = T.pack . show
  toQueryParam = T.pack . show


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

instance ToHttpApiData Color where
  toUrlPiece = T.pack . show
  toQueryParam = T.pack . show

data CellType
    = Std
    | Link
    | Text
    | Fixed
    deriving (Show, Read, Eq, Generic, Enum, Bounded)

instance FromHttpApiData CellType where
  parseUrlPiece :: T.Text -> Either T.Text CellType
  parseUrlPiece = myParse

instance ToHttpApiData CellType where
  toUrlPiece = T.pack . show
  toQueryParam = T.pack . show

myParse :: (Show a, Enum a, Bounded a) => T.Text -> Either T.Text a
myParse s = maybe (Left "cannot find enum value") Right $ M.lookup s m
  where m = M.fromList $ map (\v -> (T.pack $ show v, v)) [minBound..maxBound]


-- TH invocations for persistent
derivePersistField "Env"
derivePersistField "Color"
derivePersistField "CellType"

-- TH invocation for servant-elm
deriveBoth defaultOptions ''Env
deriveBoth defaultOptions ''Color
deriveBoth defaultOptions ''CellType
