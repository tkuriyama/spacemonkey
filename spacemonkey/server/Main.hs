{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import Prelude.Compat

-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.Aeson
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
-- import Data.ByteString (ByteString)
-- import Data.List
-- import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Proxy
-- import Data.String.Conversions
-- import Data.Time.Calendar
-- import GHC.Generics
-- import Lucid
-- import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import Servant.Types.SourceT (source)
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html

-- data ServerState = ServerState { state :: String
--                                , counter :: Int }
--   deriving (Generic)

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
