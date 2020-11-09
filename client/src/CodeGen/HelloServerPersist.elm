module CodeGen.HelloServerPersist exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias ServerState  =
   { serverStateState: String
   , serverStateCounter: Int
   }

jsonDecServerState : Json.Decode.Decoder ( ServerState )
jsonDecServerState =
   Json.Decode.succeed (\pserverStateState pserverStateCounter -> {serverStateState = pserverStateState, serverStateCounter = pserverStateCounter})
   |> required "serverStateState" (Json.Decode.string)
   |> required "serverStateCounter" (Json.Decode.int)

jsonEncServerState : ServerState -> Value
jsonEncServerState  val =
   Json.Encode.object
   [ ("serverStateState", Json.Encode.string val.serverStateState)
   , ("serverStateCounter", Json.Encode.int val.serverStateCounter)
   ]


getRequest : (Result Http.Error  ((Maybe ServerState))  -> msg) -> Cmd msg
getRequest toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "request"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecServerState))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putIncrement : (Result Http.Error  ((Maybe ServerState))  -> msg) -> Cmd msg
putIncrement toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "increment"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecServerState))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
