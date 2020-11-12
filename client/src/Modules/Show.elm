module Modules.Show exposing (..)

import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)

import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

show : Model -> Html msg
show m = div [] [showGrid m.grid]

showGrid : Grid -> Html msg
showGrid grid = div [] (List.map showCell grid)

showCell : CSP.Cell -> Html msg
showCell cell = div [] []
