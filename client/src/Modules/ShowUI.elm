module Modules.ShowUI exposing (popup)

import Bulma.Components as BulmaC
import Bulma.Elements as BulmaE
import Bulma.Form as BulmaF
import Bulma.Modifiers as BulmaM

import Html as Html exposing (Attribute, Html, div)
import Html.Attributes as HtmlA
import Html.Events as HtmlE

-- import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.BulmaAssets as BulmaAssets
import Modules.Types exposing (..)

--------------------------------------------------------------------------------
-- Bulma types

type alias Button msg = Html msg
type alias Control msg = Html msg
type alias Modal msg = Html msg
type alias IsModalOpen = Bool

--------------------------------------------------------------------------------

popup : IsModalOpen -> String -> Html Msg
popup isOpen cellValue =
    BulmaC.modal
        isOpen
        []
        [ BulmaC.modalBackground
              [ HtmlE.onClick ClickCancelClosePopup
              , HtmlA.style "opacity" "0.5" ]
              []
        , BulmaC.modalContent
              [ HtmlA.style "background-color" "darkgrey"
              , HtmlA.style "font-family" "Consolas, monaco, monospace"
              ]
              [ Html.br [] []
              , Html.div
                    [ HtmlA.style "font-size" "25px"
                    , HtmlA.style "margin" "10px" ]
                    [ Html.text "Edit Cell Value" ]
              , Html.br [] [] 
              , popupInput cellValue
              , Html.br [] []
              , div
                    [ HtmlA.style "float" "right"
                    ]
                    [ popupCancelButton
                    , popupOkButton
                    ]
            ]
        , BulmaC.modalClose
            BulmaM.Large
            [ HtmlE.onClick ClickCancelClosePopup ]
            []
        ]

popupInput : String -> Control Msg
popupInput val =
    BulmaF.controlText
        BulmaAssets.popupInputMods
        []
        [ HtmlE.onInput (\s -> UpdateUserBuffer s)
        , HtmlA.style "font-family" "Consolas, monaco, monospace"
        , HtmlA.value val
        ]
        []

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
