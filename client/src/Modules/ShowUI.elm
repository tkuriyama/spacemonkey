module Modules.ShowUI exposing (popup)

import Bulma.Components as BulmaC
import Bulma.Elements as BulmaE
import Bulma.Form as BulmaF
import Bulma.Modifiers as BulmaM

import Html as Html exposing (Attribute, Html, div)
import Html.Attributes as HtmlA
import Html.Events as HtmlE

import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.BulmaAssets as BulmaAssets
import Modules.Types exposing (..)

--------------------------------------------------------------------------------
-- Bulma types

type alias Button msg = Html msg
type alias Control msg = Html msg
type alias Modal msg = Html msg
type alias IsModalOpen = Bool

--------------------------------------------------------------------------------

popup : IsModalOpen -> CSP.CellType -> String -> Html Msg
popup isOpen ctype cellValue =
    BulmaC.modal
        isOpen
        []
        [ BulmaC.modalBackground
              [ HtmlE.onClick ClickCancelClosePopup
              , HtmlA.style "opacity" "0.5"
              ]
              []
        , case ctype of
              CSP.Std -> stdModalContent cellValue
              CSP.Link -> linkModalContent cellValue
              _ -> textModalContent cellValue
        , BulmaC.modalClose
            BulmaM.Large
            [ HtmlE.onClick ClickCancelClosePopup ]
            []
        ]

popupButtons : Html Msg
popupButtons =
    div
    [ HtmlA.style "float" "right" ]
    [ popupCancelButton
    , popupOkButton
    ]

popupCancelButton : BulmaE.Button Msg
popupCancelButton =
    BulmaE.button
        BulmaAssets.cancelButtonMods
        [ HtmlE.onClick ClickCancelClosePopup
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        ]
        [ Html.text "Cancel" ]

popupOkButton : BulmaE.Button Msg
popupOkButton =
    BulmaE.button
        BulmaAssets.okButtonMods
        [ HtmlE.onClick ClickClosePopup
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        ]
        [ Html.text "Save" ]

--------------------------------------------------------------------------------

stdModalContent : String -> BulmaC.ModalPartition Msg
stdModalContent cellValue =
    BulmaC.modalContent
        [ HtmlA.style "background-color" "darkgrey"
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        ]
    [ Html.br [] []
    , Html.div
          [ HtmlA.style "font-size" "25px"
          , HtmlA.style "margin" "10px" ]
          [ Html.text "Cell Message" ]
    , Html.br [] []
    , stdModalInput cellValue
    , Html.br [] []
    , popupButtons
    ]

stdModalInput : String -> Control Msg
stdModalInput val =
    BulmaF.controlText
        BulmaAssets.stdInputMods
        []
        [ HtmlE.onInput (\s -> UpdateUserBuffer s)
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        , HtmlA.value val
        ]
        []

--------------------------------------------------------------------------------

linkModalContent : String -> BulmaC.ModalPartition Msg
linkModalContent cellValue =
    BulmaC.modalContent
        [ HtmlA.style "background-color" "darkgrey"
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        ]
    [ Html.br [] []
    , Html.div
          [ HtmlA.style "font-size" "25px"
          , HtmlA.style "margin" "10px" ]
          [ Html.text "Cell Link" ]
    , Html.br [] []
    , linkModalInput cellValue
    , Html.br [] []
    , Html.p
        [ HtmlA.style "margin" "20px"
        , HtmlA.style "font-size" "25px"
        , HtmlA.style "word-wrap" "break-word"
        ]
        [ Html.a
              [ HtmlA.target "_blank"
              , HtmlA.href cellValue
              ]
              [ Html.text cellValue ]
        ]
    , Html.br [] []
    , Html.br [] []
    , popupButtons
    ]

linkModalInput : String -> Control Msg
linkModalInput val =
    BulmaF.controlText
        BulmaAssets.linkInputMods
        []
        [ HtmlE.onInput (\s -> UpdateUserBuffer s)
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        , HtmlA.value val
        ]
        []

--------------------------------------------------------------------------------

textModalContent : String -> BulmaC.ModalPartition Msg
textModalContent cellValue =
    BulmaC.modalContent
        [ HtmlA.style "background-color" "darkgrey"
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        ]
    [ Html.br [] []
    , Html.div
          [ HtmlA.style "font-size" "25px"
          , HtmlA.style "margin" "10px" ]
          [ Html.text "Edit Cell Message" ]
    , Html.br [] []
    , stdModalInput cellValue
    , Html.br [] []
    , popupButtons
    ]

