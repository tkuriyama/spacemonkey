module Server.HelloServerAcid where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, ReaderT, runReaderT)
-- import           Control.Monad.State  (get, put)
-- import           Control.Exception (bracket)

-- import           Data.IORef
import           Data.Proxy

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API 
import           Servant.Server

import           Data.Acid
-- import           Data.SafeCopy

import qualified Modules.HelloServerAcid as HSA

type ReaderHandler = ReaderT Env Handler

data Env = Env
  { state :: AcidState HSA.ServerState }

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
  (Env st) <- ask
  liftIO $ query st HSA.QueryState

updateServer :: Int -> ReaderHandler HSA.ServerState
updateServer ctr' = do
  (Env st) <- ask
  (HSA.ServerState s _) <- liftIO $ query st HSA.QueryState
  liftIO $ update st (HSA.WriteState s ctr')

--------------------------------------------------------------------------------

main :: IO ()
main = do
  st <- openLocalStateFrom "db" (HSA.ServerState "Hello" 42)
  let env = Env { state = st }
  run 8080 $ app env
  closeAcidState st
