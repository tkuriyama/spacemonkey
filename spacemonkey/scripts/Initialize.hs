module Initialize where

import           Control.Monad (forM_)
-- import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import qualified Data.Text as T
import           System.Environment (getArgs)

import           Database.Persist.Sqlite

import qualified Modules.Spacemonkey as SP
import qualified Helpers.SpacemonkeyEnum as SPE

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then
    putStrLn "No path to DB provided" >> pure ()
    else initialize $ head args

initialize :: FilePath -> IO ()
initialize fpath =
  runNoLoggingT $ withSqliteConn (T.pack fpath) $ runSqlConn $ do
    runMigration SP.migrateAll
    -- Dev
    devId <- insert $ SP.World SPE.Dev width height
    forM_ [1..width] $ \i -> do
      forM_ [1..height] $ \j -> do
        _ <- insert $ SP.Cell devId i j SPE.White (T.pack "") SPE.Std
        pure ()
    -- Prod
    prodId <- insert $ SP.World SPE.Prod width height
    forM_ [1..width] $ \i -> do
      forM_ [1..height] $ \j -> do
        _ <- insert $ SP.Cell prodId i j SPE.White (T.pack "") SPE.Std
        pure ()
  where width = 100
        height = 50
