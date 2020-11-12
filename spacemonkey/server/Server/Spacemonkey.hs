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
server pool =
  getWorldId' :<|>
  getWorld' :<|>
  getCell' :<|>
  getMsgs' :<|>
  getUsers' :<|>
  setCellColor'
  where
    getWorldId' = liftIO . getWorldId pool
    getWorld' = liftIO . getWorld pool
    getCell' = liftIO . getCell pool
    getMsgs' wid n = liftIO $ getMsgs pool wid n
    getUsers' = liftIO . getUsers pool
    setCellColor' wid x y c = liftIO $ setCellColor pool wid x y c

getWorldId :: CP -> SPE.Environment -> IO (Maybe (SP.Key SP.World))
getWorldId pool env = flip runSqlPersistMPool pool $ do
  ret <- getBy $ SP.UniqueEnv env
  case ret of
    Nothing -> pure Nothing
    (Just (Entity wid _)) -> pure $ Just wid

getWorld :: CP -> SP.WorldId -> IO (Maybe SP.World)
getWorld pool wid = flip runSqlPersistMPool pool $ do
  ret <- get wid
  pure ret

getCell :: CP -> SP.Key SP.World -> IO [SP.Cell]
getCell pool wid = flip runSqlPersistMPool pool $ do
  entity <- selectList [SP.CellEnv ==. wid] []
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

setCellColor :: CP -> SP.Key SP.World -> Int -> Int -> SPE.Color -> IO SPE.Color
setCellColor pool wid x y c = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.CellEnv ==. wid, SP.CellX ==. x, SP.CellY ==. y]
      [SP.CellColor =. c]
  pure c


