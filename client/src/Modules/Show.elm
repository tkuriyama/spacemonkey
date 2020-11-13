module Modules.Show exposing (..)

import Array2D as A

import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, r, rx,
                                     fill, fillOpacity, opacity,
                                     stroke, strokeWidth, class,
                                     width, height, viewBox)
import TypedSvg.Types exposing (Paint(..), px, Opacity(..))
import TypedSvg.Core exposing (Svg, text)


import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

show : Model -> Html msg
show m =
    let cellSide = 20
        xCells = m.windowWidth // cellSide
        yCells = m.windowHeight // cellSide
    in div [] [showGrid A.empty]

showGrid : Grid -> Html msg
showGrid grid = div [] []
