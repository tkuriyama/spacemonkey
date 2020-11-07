module Server.HelloServerAcid where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, ReaderT, runReaderT)
import           Control.Exception (bracket)

-- import           Data.IORef
import           Data.Proxy

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server

import           Data.Acid

import qualified Modules.HelloServerAcid as HSA

type ReaderHandler = ReaderT Env Handler

data Env = Env
  { acidDB :: AcidState HSA.ServerState }

app :: Env -> Application
app env = serve servantAPI (server env)

servantAPI :: Proxy HSA.ServantAPI
servantAPI = Proxy

server :: Env -> Server HSA.ServantAPI
server env = hoistServer servantAPI (flip runReaderT env) serverT

serverT :: ServerT HSA.ServantAPI ReaderHandler
serverT = queryServer :<|> updateServer

queryServer :: ReaderHandler HSA.ServerState
queryServer = do
  Env db <- ask
  liftIO $ query db HSA.QueryState

updateServer :: Int -> ReaderHandler HSA.ServerState
updateServer ctr' = do
  Env db <- ask
  HSA.ServerState s _ <- liftIO $ query db HSA.QueryState
  let st = HSA.ServerState s ctr'
  liftIO $ update db (HSA.WriteState st)
  pure st

--------------------------------------------------------------------------------

main :: IO ()
main = bracket
  (openLocalStateFrom "db" (HSA.ServerState "Hello" 42))
  (\acid -> createCheckpoint acid >> closeAcidState acid)
  (\acid -> let env = Env { acidDB = acid }
            in run 8080 $ app env)
