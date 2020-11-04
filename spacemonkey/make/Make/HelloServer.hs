module Make.HelloServer where

import           Data.Text.Internal as TI

import           Servant.Elm  (DefineElm (DefineElm),
                               ElmOptions (..),
                               Proxy (Proxy),
                               UrlPrefix (..),
                               defElmImports, defElmOptions,
                               generateElmModuleWith)

import qualified Modules.HelloServer as HS

make :: TI.Text -> IO ()
make baseURL =
  generateElmModuleWith
    defElmOptions { urlPrefix = Static $ baseURL }
    [ "CodeGen"
    , "HelloServer"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy HS.ServerState)
    ]
    (Proxy :: Proxy HS.ServantAPI)
