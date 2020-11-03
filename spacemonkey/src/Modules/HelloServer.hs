module Modules.HelloServer where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable
import           System.Environment

import Elm.Derive
import Elm.Module
import Data.Proxy

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

data HelloServerState = HelloServerState String
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''HelloServerState)
deriveBoth defaultOptions ''HelloServerState

------------------------------------------------------
-- The transaction we will execute over the state.

writeState :: String -> Update HelloServerState ()
writeState newValue
    = put (HelloServerState newValue)

queryState :: Query HelloServerState String
queryState = do HelloServerState string <- ask
                return string

$(makeAcidic ''HelloServerState ['writeState, 'queryState])

------------------------------------------------------
-- This is how AcidState is used:

-- main :: IO ()
-- main = do acid <- openLocalState (HelloServerState "Hello world")
--           args <- getArgs
--           if null args
--              then do string <- query acid QueryState
--                      putStrLn $ "The state is: " ++ string
--              else do update acid (WriteState (unwords args))
--                      putStrLn "The state has been modified!"
