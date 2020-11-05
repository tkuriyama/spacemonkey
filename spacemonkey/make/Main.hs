module Main where

import qualified Make.HelloServer as HSM
import qualified Make.HelloServerAcid as HSA

main :: IO ()
main = do
  HSM.make "http://localhost:8080"
  HSA.make "http://localhost:8080"
