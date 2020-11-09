module Make.HelloServerPersist where

import           Data.Text.Internal as TI

import           Servant.Elm  (DefineElm (DefineElm),
                               ElmOptions (..),
                               Proxy (Proxy),
                               UrlPrefix (..),
                               defElmImports, defElmOptions,
                               generateElmModuleWith)

import qualified Modules.HelloServerPersist as HSP

make :: TI.Text -> IO ()
make baseURL =
  generateElmModuleWith
    defElmOptions { urlPrefix = Static $ baseURL }
    [ "CodeGen"
    , "HelloServerPersist"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy HSP.ServerState)
    ]
    (Proxy :: Proxy HSP.ServantAPI)
