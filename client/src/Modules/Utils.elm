
module Modules.Utils exposing (..)

import Http exposing (Error)

import CodeGen.Spacemonkey as CSP
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

getDeltas : CSP.Direction -> (Int, Int)
getDeltas dir = case dir of
                     CSP.North -> (0, -1)
                     CSP.South -> (0, 1)
                     CSP.East -> (1, 0)
                     CSP.West -> (-1, 0)

getFacing : Grid -> Point -> CSP.Direction -> CSP.Cell
getFacing grid (x, y) dir =
    let (dx, dy) = getDeltas dir
        (x_, y_) = (x + dx, y + dy)
        f c acc = if (c.cellX, c.cellY) == (x_, y_) then c else acc
    in List.foldr f defaultCell grid

eqCoord : CSP.Cell -> CSP.Cell -> Bool
eqCoord c1 c2 = c1.cellX == c2.cellX && c1.cellY == c2.cellY

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
