module Modules.HelloServer where


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
