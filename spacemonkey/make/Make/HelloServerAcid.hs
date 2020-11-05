module Make.HelloServerAcid where

import           Data.Text.Internal as TI

import           Servant.Elm  (DefineElm (DefineElm),
                               ElmOptions (..),
                               Proxy (Proxy),
                               UrlPrefix (..),
                               defElmImports, defElmOptions,
                               generateElmModuleWith)

import qualified Modules.HelloServerAcid as HSA

make :: TI.Text -> IO ()
make baseURL =
  generateElmModuleWith
    defElmOptions { urlPrefix = Static $ baseURL }
    [ "CodeGen"
    , "HelloServerAcid"
    ]
    defElmImports
    "./"
    [ DefineElm (Proxy :: Proxy HSA.ServerState)
    ]
    (Proxy :: Proxy HSA.ServantAPI)
