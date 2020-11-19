module Modules.ShowGrid exposing (show)

import Html as Html exposing (Html, div)
import Html.Attributes as HtmlA

import Color as Color
import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, r, rx,
                                     fill, fillOpacity, opacity,
                                     stroke, strokeWidth, class,
                                     fontSize, fontFamily, fontWeight,
                                     width, height, rotate,
                                     textAnchor, dominantBaseline,
                                     viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (px, Paint(..), Opacity(..),
                                FontWeight(..),
                                AnchorAlignment (..), DominantBaseline (..))

import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.BulmaAssets as BulmaAssets
import Modules.Camera as Camera
import Modules.Types exposing (..)
import Modules.Utils as Utils

--------------------------------------------------------------------------------

show : Model -> Html msg
show m =
    let grid = Camera.getVisible m.viewOpts.camera m.grid
              |> Camera.projectGrid m.viewOpts.camera
        users = m.self :: m.others
              |> Camera.projectUsers m.viewOpts.camera
        (w, h) = (m.viewOpts.windowWidth, m.viewOpts.windowHeight)
        cellSize = toFloat m.viewOpts.cellSize
    in div
       []
       [ svg
         [ viewBox 0 0 (toFloat w) (toFloat h)]
         ( showGrid cellSize grid ++
           showUsers cellSize users
         )
       ]

--------------------------------------------------------------------------------

showGrid : Float -> List CSP.Cell -> List (Svg msg)
showGrid cellSize = List.concatMap (showCell cellSize)

showCell : Float -> CSP.Cell -> List (Svg msg)
showCell cellSize c =
    let (cellX, cellY) =
            (toFloat c.cellX * cellSize, toFloat c.cellY * cellSize)
    in [ rect
             [ x <| px <| cellX
             , y <| px <| cellY
             , width <| px cellSize
             , height <| px cellSize
             , stroke <| Paint Color.grey
             , strokeWidth <| px 1
             , fill <| Paint <| mapColor c.cellColor
             ]
             []
       , text_
             [ x <| px <| cellX + cellSize * 0.5
             , y <| px <| cellY + cellSize - cellSize * 0.45
             , fontSize <| px <| cellSize * 0.85
             , fontFamily ["Consolas", "monaco", "monospace"]
             , textAnchor AnchorMiddle
             , dominantBaseline DominantBaselineMiddle
             , fill <| Paint Color.white
             ]
             [ showCellContent c.cellCType c.cellValue
             ]
       ]

showCellContent : CSP.CellType -> String -> Html msg
showCellContent ctype s =
    case ctype of
        CSP.Std -> text <| Utils.stringHead s
        CSP.Link -> text "🔗"
        CSP.Text -> text "📝"
        _ -> text ""

mapColor : CSP.Color -> Color.Color
mapColor color =
    case color of
        CSP.White -> Color.white
        CSP.Yellow -> Color.rgb255 252 169 3
        CSP.Red -> Color.rgb255 235 47 47
        CSP.Green -> Color.green
        CSP.Blue -> Color.rgb255 121 176 242
        CSP.Grey -> Color.rgb255 217 217 217

--------------------------------------------------------------------------------

showUsers : Float -> List CSP.User -> List (Svg msg)
showUsers cellSize = List.concatMap (showUser cellSize)

showUser: Float -> CSP.User -> List (Svg msg)
showUser cellSize u =
    let dirSize = 3
        (userX, userY) =
            (toFloat u.userX * cellSize, toFloat u.userY * cellSize)
        ((dirX1, dirX2), (dirW, dirH)) =
            case u.userFacing of
                East -> ((userX + cellSize - dirSize, userY)
                        , (dirSize, cellSize))
                West -> ((userX, userY), (dirSize, cellSize))
                North -> ((userX, userY), (cellSize, dirSize))
                South -> ((userX, userY + cellSize - dirSize)
                         , (cellSize, dirSize))
    in [text_
             [ x <| px <| userX + cellSize * 0.5
             , y <| px <| userY + cellSize - cellSize * 0.4 -- text draws up
             , fontSize <| px <| cellSize * 0.8
             , textAnchor AnchorMiddle
             , dominantBaseline DominantBaselineMiddle
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

