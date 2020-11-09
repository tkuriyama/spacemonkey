module Main where

import qualified Make.HelloServer as HSM
import qualified Make.HelloServerAcid as HSA
import qualified Make.HelloServerPersist as HSP

main :: IO ()
main = do
  HSM.make "http://localhost:8080"
  HSA.make "http://localhost:8080"
  HSP.make "http://localhost:8080"
