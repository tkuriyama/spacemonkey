module Make.Spacemonkey where

import           Data.Text.Internal as TI

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
    defElmOptions { urlPrefix = Static $ baseURL }
    [ "CodeGen"
    , "Spacemonkey"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy SP.World)
    , DefineElm (Proxy :: Proxy SP.Cell)
    , DefineElm (Proxy :: Proxy SP.Message)
    , DefineElm (Proxy :: Proxy SP.User)
    , DefineElm (Proxy :: Proxy SPE.Environment)
    , DefineElm (Proxy :: Proxy SPE.Color)
    , DefineElm (Proxy :: Proxy SPE.CellType)
    ]
    (Proxy :: Proxy SP.API)
