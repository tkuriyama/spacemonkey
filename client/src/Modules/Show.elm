module Modules.Show exposing (..)

import Array2D as A

import Color
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, r, rx,
                                     fill, fillOpacity, opacity,
                                     stroke, strokeWidth, class,
                                     width, height, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), px, Opacity(..))


import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

show : Model -> Html msg
show m =
    let cellSide = 20
        xCells = m.windowWidth // cellSide
        yCells = m.windowHeight // cellSide
        cs = m.grid --List.take (xCells * yCells) m.grid
    in div
       []
       [ svg
         [ viewBox 0 0 (toFloat m.windowWidth) (toFloat m.windowHeight) ]
         (showGrid cellSide cs) ]

showGrid : Int -> Grid -> List (Svg msg)
showGrid cellSide grid = List.map (showCell cellSide) grid

showCell : Int -> CSP.Cell -> Svg msg
showCell cellSide c =
    let cellSide_ = toFloat cellSide
        (x1_, y1_) = toCoords (c.cellX, c.cellY) cellSide_
    in rect
        [ x <| px x1_
        , y <| px <| y1_
        , width <| px cellSide_
        , height <| px cellSide_
        , stroke <| Paint Color.grey
        , strokeWidth <| px 1
        , fill <| Paint <| mapColor c.cellColor
        ]
        []

toCoords : (Int, Int) -> Float -> (Float, Float)
toCoords (xInd, yInd) len = (toFloat xInd * len, toFloat yInd * len)

mapColor : CSP.Color -> Color.Color
mapColor color =
    case color of
        White -> Color.white
        Yellow -> Color.yellow
        Red -> Color.red
        Green -> Color.green
        Blue -> Color.blue
        Grey -> Color.darkGrey
