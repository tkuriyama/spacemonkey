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
    let grid = Camera.getVisible m.viewOpts.camera m.grid
              |> Camera.projectGrid m.viewOpts.camera
        users = m.self :: m.others
              |> Camera.projectUsers m.viewOpts.camera
        (w, h) = (m.viewOpts.windowWidth, m.viewOpts.windowHeight)
        cellSize = m.viewOpts.cellSize
    in div
       []
       [ svg
         [ viewBox 0 0 (toFloat w) (toFloat h)]
         ( showGrid cellSize grid ++
           showUsers cellSize users
         )
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
showUsers cellSize = List.concatMap (showUser cellSize)

showUser: Int -> CSP.User -> List (Svg msg)
showUser cellSize u =
    let dirSize = 3
        cellSize_ = toFloat cellSize
        project n = toFloat n * cellSize_
        (userX, userY) = (project u.userX, project u.userY)
        ((dirX1, dirX2), (dirW, dirH)) =
            case u.userFacing of
                East -> ((userX + cellSize_ - dirSize, userY)
                        , (dirSize, cellSize_))
                West -> ((userX, userY), (dirSize, cellSize_))
                North -> ((userX, userY), (cellSize_, dirSize))
                South -> ((userX, userY + cellSize_ - dirSize)
                         , (cellSize_, dirSize))
    in [ text_
             [ x <| px <| userX + cellSize_ * 0.05
             , y <| px <| userY + cellSize_ - cellSize_ * 0.1 -- text draws up
             , fontSize <| px <| cellSize_ * 0.85
             ]
             [ text u.userName ]
       , rect
             [ x <| px dirX1
             , y <| px dirX2
             , width <| px dirW
             , height <| px dirH
             , stroke <| Paint Color.black
             , strokeWidth <| px 1
             , fill <| Paint Color.white ]
             []
       ]
