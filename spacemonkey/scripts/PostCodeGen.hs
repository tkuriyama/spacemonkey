{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostCodeGen where

import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile, writeFile)
import           System.Environment (getArgs)

import qualified Helpers.SpacemonkeyEnum as SPE 

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then
    putStrLn "No path to file provided" >> pure ()
    else appendMain $ head args

appendMain :: FilePath -> IO ()
appendMain fpath = do
  ft <- TIO.readFile fpath
  let names = ["World", "Cell", "Message", "User"]
      enums = ["Env", "Color", "CellType", "Direction"]
      ft' = replace' names enums $ append' names ft
  TIO.writeFile fpath ft'

--------------------------------------------------------------------------------

append' :: [T.Text] -> T.Text -> T.Text
append' names t =
  t <>
  comment <> br <>
  aliases <> br <>
  jsonHandlers <> br <>
  sumToString (Proxy :: Proxy SPE.Env) "Env" <> br <>
  sumToString (Proxy :: Proxy SPE.Color) "Color" <> br <>
  sumToString (Proxy :: Proxy SPE.CellType) "CellType" <> br <>
  sumToString (Proxy :: Proxy SPE.Direction) "Direction" <> br
  where
    br = "\n"
    comment =
      "\n-- Post Code Gen Appends (after servant-elm)\n" <>
      "\n-- Add aliases to resolve erasure of (Key a) types from persistent\n"
    aliases =
      T.concat ["type alias " <> x <> "Id = Int\n" | x <- names]
    jsonHandlers =
      T.concat ["jsonEnc" <> x <> "Id = Json.Encode.int\n" <>
                "jsonDec" <> x <> "Id = Json.Decode.int\n" | x <- names]

sumToString :: forall a. (Show a, Enum a, Bounded a) => Proxy a -> T.Text ->
               T.Text
sumToString Proxy name =
  "strEnc" <> name <> " : " <> name <> " -> String\n" <>
  "strEnc" <> name <> " val = \n" <>
  "    case val of \n" <>
  T.concat pairs
  where
    pairs = map f $ enumerate @a
    f v = let v' = T.pack $ show v
          in T.replicate 8 " " <> v' <> " -> \"" <> v' <> "\"\n"

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

--------------------------------------------------------------------------------

-- Text.Replace will replace all non-verlapping instances of old with new
replace' :: [T.Text] -> [T.Text] -> T.Text -> T.Text
replace' names enums t = foldr f t replacements
  where
    f (old, new) acc =
      T.replace old new acc
    replacements =
      [( "Key " <> x,
         x <> "Id" )
       | x <- names] ++
      [( "((jsonDecKey jsonDec" <> x <> "))"
       ,  "jsonDec" <> x <> "Id" )
       | x <- names] ++
      [( "capture_" <> T.toLower x <> " |> String.fromInt"
       , "capture_" <> T.toLower x <> " |> strEnc" <> x )
       | x <- enums]

