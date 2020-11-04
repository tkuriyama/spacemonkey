module HelloServer exposing(..)

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
   { state: String
   , counter: Int
   }

jsonDecServerState : Json.Decode.Decoder ( ServerState )
jsonDecServerState =
   Json.Decode.succeed (\pstate pcounter -> {state = pstate, counter = pcounter})
   |> required "state" (Json.Decode.string)
   |> required "counter" (Json.Decode.int)

jsonEncServerState : ServerState -> Value
jsonEncServerState  val =
   Json.Encode.object
   [ ("state", Json.Encode.string val.state)
   , ("counter", Json.Encode.int val.counter)
   ]


getServer : (Result Http.Error  (ServerState)  -> msg) -> Cmd msg
getServer toMsg =
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
                    [ "server"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecServerState
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putSetCounterByCounter : Int -> (Result Http.Error  (ServerState)  -> msg) -> Cmd msg
putSetCounterByCounter capture_counter toMsg =
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
                    [ "setCounter"
                    , (capture_counter |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecServerState
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
