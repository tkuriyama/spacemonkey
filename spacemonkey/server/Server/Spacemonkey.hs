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
  getUser' :<|>
  getUsers' :<|>
  move' :<|>
  reface' :<|>
  setCellColor' :<|>
  setCellCType' :<|>
  clearCellValue' :<|>
  setCellValue'
  where
    getWorldId' = liftIO . getWorldId pool
    getWorld' = liftIO . getWorld pool
    getCell' = liftIO . getCell pool
    getMsgs' wid n = liftIO $ getMsgs pool wid n
    getUser' = liftIO . getUser pool
    getUsers' = liftIO . getUsers pool
    move' uid dir = liftIO $ move pool uid dir
    reface' uid dir = liftIO $ reface pool uid dir
    setCellColor' wid x y c = liftIO $ setCellColor pool wid x y c
    setCellCType' wid x y ct = liftIO $ setCellCType pool wid x y ct
    clearCellValue' wid x y = liftIO $ clearCellValue pool wid x y
    setCellValue' wid x y v = liftIO $ setCellValue pool wid x y v

getWorldId :: CP -> SPE.Env -> IO (Maybe (SP.Key SP.World))
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

getUser :: CP -> SP.Key SP.User -> IO (Maybe SP.User)
getUser pool uid = flip runSqlPersistMPool pool $ do
  ret <- get uid
  pure ret

getUsers :: CP -> SP.Key SP.World -> IO [SP.User]
getUsers pool wid = flip runSqlPersistMPool pool $ do
  entity <- selectList [SP.UserEnv ==. wid] []
  pure $ entityVal <$> entity

move :: CP -> SP.Key SP.User -> SPE.Direction -> IO SPE.Direction
move pool uid dir = do
  flip runSqlPersistMPool pool $
    case dir of
      SPE.North -> update uid [SP.UserY -=. 1]
      SPE.South -> update uid [SP.UserY +=. 1]
      SPE.East -> update uid [SP.UserX +=. 1]
      SPE.West -> update uid [SP.UserX -=. 1]
  pure dir

reface :: CP -> SP.Key SP.User -> SPE.Direction -> IO SPE.Direction
reface pool uid dir = do
  flip runSqlPersistMPool pool $
    update uid [SP.UserFacing =. dir]
  pure dir

setCellColor :: CP -> SP.Key SP.World -> Int -> Int -> SPE.Color -> IO SPE.Color
setCellColor pool wid x y c = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.CellEnv ==. wid, SP.CellX ==. x, SP.CellY ==. y]
      [SP.CellColor =. c]
  pure c

setCellCType :: CP -> SP.Key SP.World -> Int -> Int -> SPE.CellType ->
                IO SPE.CellType
setCellCType pool wid x y ct = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.CellEnv ==. wid, SP.CellX ==. x, SP.CellY ==. y]
      [SP.CellCType =. ct]
  pure ct

clearCellValue :: CP -> SP.Key SP.World -> Int -> Int -> IO Bool
clearCellValue pool wid x y = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.CellEnv ==. wid, SP.CellX ==. x, SP.CellY ==. y]
      [SP.CellValue =. ""]
  pure True

setCellValue :: CP -> SP.Key SP.World -> Int -> Int -> T.Text -> IO T.Text
setCellValue pool wid x y v = do
  flip runSqlPersistMPool pool $
    updateWhere
      [SP.CellEnv ==. wid, SP.CellX ==. x, SP.CellY ==. y]
      [SP.CellValue =. v]
  pure v

