module Main where

-- import qualified Server.HelloServer as HS
-- import qualified Server.HelloServerAcid as HSA
import qualified Server.HelloServerPersist as HSP

main :: IO ()
main = HSP.main
