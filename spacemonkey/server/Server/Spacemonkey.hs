module Server.Spacemonkey where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (runStderrLoggingT)

import qualified Data.Text as T
import           Data.Proxy

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server

import           Database.Persist.Sqlite

import qualified Modules.Spacemonkey as SP
import qualified Helpers.SpacemonkeyEnum as SPE

--------------------------------------------------------------------------------

main :: IO ()
main = bracket
  (mkApp "dbsqlite/spacemonkey.db")
  (\_ -> pure ())
  (\app' -> run 8080 $ app')

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (T.pack sqliteFile) 10
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve servantAPI $ server pool

servantAPI :: Proxy SP.API
servantAPI = Proxy

--------------------------------------------------------------------------------

type CP = ConnectionPool

server :: CP -> Server SP.API
server pool = getWorld' :<|> getGrid' :<|> getMsgs' :<|> getUsers' :<|>
             setGridColor'
  where
    getWorld' = liftIO . getWorld pool
    getGrid' = liftIO . getGrid pool
    getMsgs' wid n = liftIO $ getMsgs pool wid n
    getUsers' = liftIO . getUsers pool
    setGridColor' wid x y c = liftIO $ setGridColor pool wid x y c

getWorld :: CP -> SPE.Environment -> IO (Maybe (SP.Key SP.World, SP.World))
getWorld pool env = flip runSqlPersistMPool pool $ do
  ret <- getBy $ SP.UniqueEnv env
  case ret of
    Nothing -> pure Nothing
    (Just (Entity wid w)) -> pure $ Just (wid, w)

getGrid :: CP -> SP.Key SP.World -> IO [SP.Grid]
getGrid pool wid = flip runSqlPersistMPool pool $ do
  entity <- selectList [SP.GridEnv ==. wid] []
  pure $ entityVal <$> entity

getMsgs  :: CP -> SP.Key SP.World -> Int -> IO [SP.Message]
getMsgs pool wid n = flip runSqlPersistMPool pool $ do
  entity <-
    selectList [SP.MessageEnv ==. wid] [Desc SP.MessageId, LimitTo n]
  pure $ entityVal <$> entity

getUsers :: CP -> SP.Key SP.World -> IO [SP.User]
getUsers pool wid = flip runSqlPersistMPool pool $ do
  entity <- selectList [SP.UserEnv ==. wid] []
  pure $ entityVal <$> entity

setGridColor :: CP -> SP.Key SP.World -> Int -> Int -> SPE.Color -> IO SPE.Color
setGridColor pool wid x y c = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.GridEnv ==. wid, SP.GridX ==. x, SP.GridY ==. y]
      [SP.GridCellColor =. c]
  pure c


