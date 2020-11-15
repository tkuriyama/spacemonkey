module Make.Spacemonkey where

import           Data.Text.Internal as TI

import           Elm.TyRep as ET
import qualified Elm.Module as Elm
import           Servant.Elm  (DefineElm (DefineElm),
                               ElmOptions (..),
                               Proxy (Proxy),
                               UrlPrefix (..),
                               defElmImports, defElmOptions,
                               generateElmModuleWith)

import qualified Modules.Spacemonkey as SP
import qualified Helpers.SpacemonkeyEnum as SPE

make :: TI.Text -> IO ()
make baseURL =
  generateElmModuleWith
    defElmOptions { urlPrefix = Static $ baseURL
                  , elmAlterations = myAlterations
                  }
    [ "CodeGen"
    , "Spacemonkey"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy SP.World)
    , DefineElm (Proxy :: Proxy SP.Cell)
    , DefineElm (Proxy :: Proxy SP.Message)
    , DefineElm (Proxy :: Proxy SP.User)
    , DefineElm (Proxy :: Proxy SPE.Env)
    , DefineElm (Proxy :: Proxy SPE.Color)
    , DefineElm (Proxy :: Proxy SPE.CellType)
    , DefineElm (Proxy :: Proxy SPE.Direction)
    ]
    (Proxy :: Proxy SP.API)

-- Resolve persistent's phantom type of Key a to Int aliases (Sqlite key type)
-- This requires further manipulation of the Elm output
-- (1) the Int aliases need to be added
-- (2) the servant API types are not replaced
myTypeAlterations :: ET.EType -> ET.EType
myTypeAlterations t = case t of
  ET.ETyApp (ET.ETyCon (ET.ETCon "Key")) (ET.ETyCon (ET.ETCon "World")) ->
    ET.ETyCon (ET.ETCon "WorldId")
  ET.ETyApp (ET.ETyCon (ET.ETCon "Key")) (ET.ETyCon (ET.ETCon "Cell")) ->
    ET.ETyCon (ET.ETCon "CellId")
  ET.ETyApp (ET.ETyCon (ET.ETCon "Key")) (ET.ETyCon (ET.ETCon "Message")) ->
    ET.ETyCon (ET.ETCon "MessageId")
  ET.ETyApp (ET.ETyCon (ET.ETCon "Key")) (ET.ETyCon (ET.ETCon "User")) ->
    ET.ETyCon (ET.ETCon "UserId")
  _ ->
    Elm.defaultTypeAlterations t

myAlterations :: ET.ETypeDef -> ET.ETypeDef
myAlterations = Elm.recAlterType myTypeAlterations
