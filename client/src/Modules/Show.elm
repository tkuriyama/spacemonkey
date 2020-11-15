module Modules.Show exposing (..)

import Color
import Html exposing (Html, div)
import Html.Attributes exposing (class, value, placeholder)
import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, r, rx,
                                     fill, fillOpacity, opacity,
                                     stroke, strokeWidth, class,
                                     fontSize,
                                     width, height, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), px, Opacity(..))


import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.Camera as Camera
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

show : Model -> Html msg
show m =
    let cs = Camera.getVisible m.viewOpts m.grid
        (w, h) = (m.viewOpts.windowWidth, m.viewOpts.windowHeight)
        cellSize = m.viewOpts.cellSize
    in div
       []
       [ svg
         [ viewBox 0 0 (toFloat w) (toFloat h)]
         (showGrid cellSize cs ++
          showUsers cellSize m.users)
       ]

--------------------------------------------------------------------------------

showGrid : Int -> List CSP.Cell -> List (Svg msg)
showGrid cellSize = List.map (showCell cellSize)

showCell : Int -> CSP.Cell -> Svg msg
showCell cellSize c =
    let cellSize_ = toFloat cellSize
        project x = toFloat x * cellSize_
    in rect
        [ x <| px <| project c.cellX
        , y <| px <| project c.cellY
        , width <| px cellSize_
        , height <| px cellSize_
        , stroke <| Paint Color.grey
        , strokeWidth <| px 1
        , fill <| Paint <| mapColor c.cellColor
        ]
        []

mapColor : CSP.Color -> Color.Color
mapColor color =
    case color of
        White -> Color.white
        Yellow -> Color.yellow
        Red -> Color.red
        Green -> Color.green
        Blue -> Color.blue
        Grey -> Color.darkGrey

--------------------------------------------------------------------------------

showUsers : Int -> List CSP.User -> List (Svg msg)
showUsers cellSize = List.map (showUser cellSize)

showUser: Int -> CSP.User -> Svg msg
showUser cellSize u =
    let cellSize_ = toFloat cellSize
        project x = toFloat x * cellSize_
    in text_
        [ x <| px <| project u.userX + cellSize_ * 0.05
        , y <| px <| project u.userY - cellSize_ * 0.10
        , fontSize <| px <| cellSize_ * 0.9
        ]
        [ text u.userName ]
