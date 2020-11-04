{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- import           Elm.Derive   (defaultOptions, deriveBoth)

-- import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), ElmOptions (..),
                               Proxy (Proxy),
                               UrlPrefix (..), defElmImports, defElmOptions,
                               generateElmModuleWith)
-- import           Data.Proxy

import qualified Modules.HelloServer as HS

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions { urlPrefix = Static $ "http://localhost:8080" }
    [ ""
    , "HelloServer"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy HS.ServerState)
    ]
    (Proxy :: Proxy HS.ServantAPI)
