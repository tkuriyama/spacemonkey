module Modules.Show exposing (show, popup)

import Bulma.Components exposing (modal, modalClose, modalBackground,
                                  modalContent)
import Bulma.Form exposing (..)
import Bulma.Elements as BulmaE
import Bulma.Modifiers as BulmaM
import Color
import Html exposing (Attribute, Html, div, i)
import Html.Attributes as HtmlA exposing (class, value, placeholder)
import Html.Events exposing (onInput)
import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, r, rx,
                                     fill, fillOpacity, opacity,
                                     stroke, strokeWidth, class,
                                     fontSize, fontFamily, fontWeight,
                                     width, height,
                                     textAnchor, dominantBaseline,
                                     viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), px, percent, Opacity(..),
                                FontWeight(..),
                                AnchorAlignment (..), DominantBaseline (..))

import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.Camera as Camera
import Modules.Types exposing (..)
import Modules.Utils as Utils

--------------------------------------------------------------------------------
-- Bulma types

type alias Control msg = Html msg
type alias Modal msg = Html msg

type alias IsModalOpen = Bool

type alias InputModifiers msg =
    { size : BulmaM.Size
    , state : BulmaM.State
    , color : BulmaM.Color
    , expanded : Bool
    , rounded : Bool
    , readonly : Bool
    , disabled : Bool
    , iconLeft :
        Maybe ( BulmaM.Size, List (Attribute msg), BulmaE.IconBody msg )
    , iconRight :
        Maybe ( BulmaM.Size, List (Attribute msg), BulmaE.IconBody msg )
    }

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
        dispText = Utils.stringHead c.cellValue
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
             -- , fontWeight FontWeightBold
             , textAnchor AnchorMiddle
             , dominantBaseline DominantBaselineMiddle
             , fill <| Paint Color.white
             ]
             [ text dispText ]
       ]

mapColor : CSP.Color -> Color.Color
mapColor color =
    case color of
        CSP.White -> Color.white
        CSP.Yellow -> Color.yellow
        CSP.Red -> Color.red
        CSP.Green -> Color.green
        CSP.Blue -> Color.blue
        CSP.Grey -> Color.darkGrey

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

--------------------------------------------------------------------------------

popup : IsModalOpen -> String -> Html Msg
popup isOpen cellValue =
    modal
        isOpen
        []
        [ modalBackground [] []
        , modalContent []
            [ cellValueInput cellValue
            ]
        , modalClose BulmaM.Large [] []
        ]

cellValueInput : String -> Control Msg
cellValueInput val =
    let controlAttrs = [ HtmlA.class "" ]
        inputAttrs   = [ onInput (\s -> UpdateUserBuffer s)
                       , HtmlA.value val
                       ]
    in controlText inputModifiers controlAttrs inputAttrs []

inputModifiers : InputModifiers msg
inputModifiers =
    { size = BulmaM.Large
    , state = BulmaM.Active
    , color = BulmaM.Default
    , expanded = True
    , rounded = True
    , readonly = False
    , disabled = False
    , iconLeft =
        Just ( BulmaM.Large, [], i [ HtmlA.class "commenting" ] []) -- TODO doesn't work?
    , iconRight = Nothing
--        Maybe ( BulmaM.Size, List (Attribute msg), BulmaE.IconBody msg )
    }

