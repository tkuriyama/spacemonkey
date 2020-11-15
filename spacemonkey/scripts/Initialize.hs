{-# LANGUAGE OverloadedStrings  #-}

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
    forM_ [SPE.Dev, SPE.Prod] $ \env -> do
      envId <- insert $ SP.World env width height

      -- regular celsl
      forM_ [1..width-2] $ \i -> do
        forM_ [1..height-2] $ \j -> do
          _ <- insert $ SP.Cell envId i j SPE.White (T.pack "") SPE.Std
          pure ()

      -- top and bottom borders
      forM_ [0..width-1] $ \i -> do
        forM_ [0, height-1] $ \j -> do
          _ <- insert $ SP.Cell envId i j SPE.Grey (T.pack "") SPE.Fixed
          pure ()

      -- left and right borders
      forM_ [0, width-1] $ \i -> do
        forM_ [1..height-2] $ \j -> do
          _ <- insert $ SP.Cell envId i j SPE.Grey (T.pack "") SPE.Fixed
          pure ()

      -- users and messages
      userId1 <- insert $ SP.User envId "🦉" 1 5 SPE.East
      userId2 <- insert $ SP.User envId "🦛" 1 6 SPE.East
      _ <- insert $ SP.Message envId userId1 "Hello, Spacemonkey!"
      _ <- insert $ SP.Message envId userId2 "Hello, Spacemonkey!"
      pure ()

  where width = 100
        height = 50

