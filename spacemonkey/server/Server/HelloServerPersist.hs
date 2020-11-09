{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes                #-}

module Server.HelloServerPersist where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.IO.Unlift
import           Control.Monad.Logger (runStderrLoggingT)
-- import           Control.Monad.Reader (ask, ReaderT, runReaderT)
import qualified Data.Text as T
import           Data.Proxy

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server

import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , entityVal, (+=.), updateWhere
                                         , insert)
-- import           Conduit

import qualified Modules.HelloServerPersist as HSP


mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (T.pack sqliteFile) 5
  runSqlPool (runMigration HSP.migrateAll) pool
  -- initialize with default value
  flip runSqlPersistMPool pool $ do
    ret <- selectFirst [HSP.ServerStateState ==. "Hello"] []
    case ret of
      Nothing -> do
        _ <- insert $ HSP.ServerState "Hello" 42
        liftIO $ return ()
      _ -> liftIO $ return ()
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve servantAPI $ server pool

servantAPI :: Proxy HSP.ServantAPI
servantAPI = Proxy

server :: ConnectionPool -> Server HSP.ServantAPI
server pool = queryServer :<|> updateServer
  where
    queryServer = liftIO runQuery
    updateServer = liftIO $ incrementCtr >> runQuery

    runQuery :: IO (Maybe HSP.ServerState)
    runQuery = flip runSqlPersistMPool pool $ do
      ret <- selectFirst [HSP.ServerStateState ==. "Hello"] []
      return $ entityVal <$> ret

    incrementCtr :: IO ()
    incrementCtr = flip runSqlPersistMPool pool $ do
      updateWhere
        [HSP.ServerStateState ==. "Hello" ]
        [HSP.ServerStateCounter +=. 1]

--------------------------------------------------------------------------------

main :: IO ()
main = bracket
  (mkApp "dbsqlite/helloserver.db")
  (\_ -> pure ())
  (\app' -> run 8080 $ app')
