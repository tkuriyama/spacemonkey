module Main where

-- import           Elm.Derive
import           Elm.Module
import           Data.Proxy

import qualified Modules.HelloServer as HS

--------------------------------------------------------------------------------

main :: IO ()
main =
    writeFile "HelloServer.elm" $ makeElmModule "HelloServer"
    [ DefineElm (Proxy :: Proxy HS.HelloServerState)
    ]
