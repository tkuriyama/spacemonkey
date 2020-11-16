
module Modules.Utils exposing (..)

import Http exposing (Error)

import CodeGen.Spacemonkey as CSP
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

moveDeltas : CSP.Direction -> (Int, Int)
moveDeltas dir = case dir of
                     CSP.North -> (0, -1)
                     CSP.South -> (0, 1)
                     CSP.East -> (1, 0)
                     CSP.West -> (-1, 0)

--------------------------------------------------------------------------------

httpError : Model -> Http.Error -> (Model, Cmd Msg)
httpError model error =
    ({ model | errorMsg = Just (buildErrorMsg error) }, Cmd.none)

buildErrorMsg : Http.Error -> String
buildErrorMsg error =
    case error of
        Http.BadUrl message ->
            message
        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."
        Http.NetworkError ->
            "Unable to reach server."
        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode
        Http.BadBody message ->
            message
