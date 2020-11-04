{-# LANGUAGE DeriveGeneric #-}

module Modules.HelloServer where

-- import           Control.Monad.Reader
-- import           Control.Monad.State
-- -- import           Data.Acid
-- -- import           Data.SafeCopy
-- -- import           Data.Typeable
-- -- import           System.Environment

-- import           Elm.Derive
-- import           Elm.Module
-- import           Data.Proxy
import           GHC.Generics
-- import           Data.Aeson (ToJSON)
import           Elm.Derive   (defaultOptions, deriveBoth)
import           Servant.API

-- ------------------------------------------------------
-- -- The Haskell structure that we want to encapsulate

data ServerState = ServerState { state :: String
                               , counter :: Int } 
    deriving (Show, Eq, Generic)

deriveBoth defaultOptions ''ServerState
-- instance ToJSON ServerState

type ServantAPI =
       "server" :> Get '[JSON] ServerState
  :<|> "setCounter" :> Capture "counter" Int :> Put '[JSON] ServerState


-- type ServerStateApi = Get '[JSON] ServerState
-- -- "books" :> Capture "bookId" Int :> Get '[JSON] Book

