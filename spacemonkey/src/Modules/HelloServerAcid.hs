module Modules.HelloServerAcid where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (put)
-- import           System.Environment   (getArgs)

import           GHC.Generics
import           Elm.Derive   (defaultOptions, deriveBoth)
import           Servant.API

import           Data.SafeCopy
import           Data.Acid

-- ------------------------------------------------------
-- -- The Haskell structure that we want to encapsulate

data ServerState = ServerState { state :: String
                               , counter :: Int }
    deriving (Show, Eq, Generic)

-- Servant definitions
deriveBoth defaultOptions ''ServerState

type ServantAPI =
       "request" :> Get '[JSON] ServerState
  :<|> "setCounter" :> Capture "counter" Int :> Put '[JSON] ServerState

-- ACid State definitions

$(deriveSafeCopy 0 'base ''ServerState)

writeState :: String -> Int -> Update ServerState ServerState
writeState s i = do
  let st = (ServerState s i)
  put st
  return st

queryState :: Query ServerState ServerState
queryState = do
  st <- ask
  return st

$(makeAcidic ''ServerState ['writeState, 'queryState])
