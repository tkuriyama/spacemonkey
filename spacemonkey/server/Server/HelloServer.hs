module Server.HelloServer where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Proxy

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server

import qualified Modules.HelloServer as HS

server :: IORef HS.ServerState -> Server HS.ServantAPI
server ref = query :<|> update
  where
  query :: Handler HS.ServerState
  query = liftIO $ readIORef ref
  update :: Int -> Handler HS.ServerState
  update counter = liftIO $ atomicModifyIORef' ref $ \st ->
    let st' = st { HS.counter = counter }
    in  (st', st')
  
servantAPI :: Proxy HS.ServantAPI
servantAPI = Proxy

app :: IORef HS.ServerState -> Application
app ref = serve servantAPI $ server ref

main :: IO ()
main = do
  ref <- newIORef $ HS.ServerState "Hello" 42
  run 8080 $ app ref
