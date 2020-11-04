module Main where

import qualified Make.HelloServer as HSM

main :: IO ()
main = do 
  HSM.make "http://localhost:8080"
