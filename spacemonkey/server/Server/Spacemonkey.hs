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

import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , entityVal, (+=.), (=.), updateWhere
                                         , insert)

import qualified Modules.Spacemonkey as SP
import qualified Helpers.SpacemonkeyEnum as SPE

--------------------------------------------------------------------------------

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (T.pack sqliteFile) 5
  runSqlPool (runMigration SP.migrateAll) pool
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

getWorld :: CP -> String -> IO (Maybe SP.World)
getWorld pool s = undefined

getGrid :: CP -> SP.Key SP.World -> IO [SP.Grid]
getGrid pool wid = undefined

getMsgs  :: CP -> SP.Key SP.World -> Int -> IO [SP.Message]
getMsgs pool wid n = undefined

getUsers :: CP -> SP.Key SP.World -> IO [SP.User]
getUsers pool wid = undefined

setGridColor :: CP -> SP.Key SP.World -> Int -> Int -> SPE.Color -> IO SPE.Color
setGridColor pool wid x y c = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.GridEnv ==. wid, SP.GridX ==. x, SP.GridY ==. y]
      [SP.GridCellColor =. c]
  pure c


  --   queryServer = liftIO runQuery
  --   updateServer = liftIO $ incrementCtr >> runQuery

  --   runQuery :: IO (Maybe SP.ServerState)
  --   runQuery = flip runSqlPersistMPool pool $ do
  --     ret <- selectFirst [SP.ServerStateState ==. "Hello"] []
  --     return $ entityVal <$> ret

  --   incrementCtr :: IO ()
  --   incrementCtr = flip runSqlPersistMPool pool $ do
  --     updateWhere
  --       [SP.ServerStateState ==. "Hello" ]
  --       [SP.ServerStateCounter +=. 1]

--------------------------------------------------------------------------------

main :: IO ()
main = bracket
  (mkApp "dbsqlite/spacemonkey.db")
  (\_ -> pure ())
  (\app' -> run 8080 $ app')
