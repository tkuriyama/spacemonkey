module Modules.ShowUI exposing (popup)

import Bulma.Components as BulmaC
import Bulma.Elements as BulmaE
import Bulma.Form as BulmaF
import Bulma.Modifiers as BulmaM

import Html as Html exposing (Attribute, Html, div, br)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Markdown.Parser as Markdown
import Markdown.Renderer

import CodeGen.Spacemonkey as CSP exposing (..)
import Modules.BulmaAssets as BulmaAssets
import Modules.Types exposing (..)

--------------------------------------------------------------------------------
-- Bulma types

type alias Button msg = Html msg
type alias Control msg = Html msg
type alias Modal msg = Html msg

--------------------------------------------------------------------------------

popup : ModalOpts -> CSP.CellType -> String -> Html Msg
popup mo ctype cellValue =
    BulmaC.modal
        mo.popupOpen
        []
        [ BulmaC.modalBackground
              [ HtmlE.onClick ClickCancelClosePopup
              , HtmlA.style "opacity" "0.5"
              ]
              []
        , case ctype of
              CSP.Std -> stdModalContent cellValue
              CSP.Link -> linkModalContent cellValue
              _ -> textModalContent cellValue mo.textEditMode
        , BulmaC.modalClose
            BulmaM.Large
            [ HtmlE.onClick ClickCancelClosePopup ]
            []
        ]

--------------------------------------------------------------------------------

stdModalContent : String -> BulmaC.ModalPartition Msg
stdModalContent cellValue =
    BulmaC.modalContent
        modalBodyAttrs
        [ modalBodyTitle "Cell Message"
        , stdModalInput cellValue
        , br [] []
        , popupButtons
        ]

stdModalInput : String -> Control Msg
stdModalInput val =
    BulmaF.controlText
        BulmaAssets.stdInputMods
        []
        ([ HtmlE.onInput (\s -> UpdateUserBuffer s)
         , HtmlA.value val
         ] ++ textStdAttrs)
        []

--------------------------------------------------------------------------------

linkModalContent : String -> BulmaC.ModalPartition Msg
linkModalContent cellValue =
    BulmaC.modalContent
        modalBodyAttrs
        [ modalBodyTitle "Cell Link"
        , linkModalInput cellValue
        , br [] []
        , div
              ([ HtmlA.style "word-wrap" "break-word" ] ++ textStdAttrs)
              [ Html.a
                    [ HtmlA.target "_blank"
                    , HtmlA.href cellValue
                    ]
                    [ Html.text cellValue ]
              ]
        , br [] []
        , br [] []
        , popupButtons
        ]

linkModalInput : String -> Control Msg
linkModalInput val =
    BulmaF.controlText
        BulmaAssets.linkInputMods
        []
        ([ HtmlE.onInput (\s -> UpdateUserBuffer s)
         , HtmlA.value val
         ] ++ textStdAttrs)
        []

--------------------------------------------------------------------------------

type alias TextBodyFunction =
    BulmaAssets.TextAreaModifiers -> String -> Control Msg

textModalContent : String -> Bool -> BulmaC.ModalPartition Msg
textModalContent cellValue textEditMode =
    case textEditMode of
        True ->
            textModalContent_
                "Edit Cell Markdown"
                textEditModalBody
                textEditButtons
                BulmaAssets.textEditAreaMods
                cellValue
        False ->
            textModalContent_
                "View Cell Markdown"
                textViewModalBody
                textViewButtons
                BulmaAssets.viewTextAreaMods
                cellValue

textModalContent_ : String -> TextBodyFunction -> Html Msg ->
                    BulmaAssets.TextAreaModifiers -> String ->
                    BulmaC.ModalPartition Msg
textModalContent_ title modalBody buttons mods cellValue =
    BulmaC.modalContent
        modalBodyAttrs
        [ modalBodyTitle title
        , modalBody mods cellValue
        , br [] []
        , buttons
        ]

textEditModalBody : BulmaAssets.TextAreaModifiers -> String -> Control Msg
textEditModalBody mods t =
    BulmaF.controlTextArea
        mods
        []
        ([ HtmlE.onInput (\s -> UpdateUserBuffer s)
        , HtmlA.value t
        ] ++ textStdAttrs)
        []

textEditButtons : Html Msg
textEditButtons =
    div
    [ HtmlA.style "float" "right"
    , HtmlA.style "margin" "10px"
    ]
    [ popupCancelButton
    , BulmaE.button
        BulmaAssets.okButtonMods
        (buttonAttrs ClickExitTextEditMode)
        [ Html.text "Save" ]
    ]

textViewModalBody : BulmaAssets.TextAreaModifiers -> String -> Html Msg
textViewModalBody mods t =
    div
    textStdAttrs
    (renderMarkdown t)

renderMarkdown : String -> List (Html Msg)
renderMarkdown t =
    let deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.deadEndToString
                |> String.join "\n"
    in case t
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Markdown.Renderer.render
                               Markdown.Renderer.defaultHtmlRenderer ast)
       of
           Ok rendered ->
               rendered
           Err errors ->
               [ Html.text errors ]

textViewButtons : Html Msg
textViewButtons =
    floatButtons "right"
        [ BulmaE.button
              BulmaAssets.editButtonMods
              (buttonAttrs ClickEnterTextEditMode)
              [ Html.text "Edit" ]
        , BulmaE.button
            BulmaAssets.cancelButtonMods
            (buttonAttrs ClickCancelClosePopup)
              [ Html.text "Close" ]
        ]

--------------------------------------------------------------------------------
-- Shared Components

modalBodyTitle : String -> Html Msg
modalBodyTitle s =
    div
    ([ HtmlA.style "margin" "10px" ] ++ textLargeAttrs)
    [ br [] []
    , Html.text s
    , br [] []]

--------------------------------------------------------------------------------
-- Shared Buttons

buttonAttrs : Msg -> List (Html.Attribute Msg)
buttonAttrs action =
    [ HtmlE.onClick action
    , HtmlA.style "font-family" "Consolas, monaco, monospace"
    ]

floatButtons : String -> List (Button Msg) -> Html Msg
floatButtons float buttons =
    div
    [ HtmlA.style "float" "right"
    , HtmlA.style "margin" "10px"
    ]
    buttons

popupButtons : Html Msg
popupButtons =
    floatButtons "right" [popupCancelButton, popupOkButton]

popupCancelButton : BulmaE.Button Msg
popupCancelButton =
    BulmaE.button
        BulmaAssets.cancelButtonMods
        (buttonAttrs ClickCancelClosePopup)
        [ Html.text "Cancel" ]

popupOkButton : BulmaE.Button Msg
popupOkButton =
    BulmaE.button
        BulmaAssets.okButtonMods
        (buttonAttrs ClickClosePopup)
        [ Html.text "Save" ]

--------------------------------------------------------------------------------
-- Shared Styles

modalBodyAttrs : List (Html.Attribute msg)
modalBodyAttrs =
    [ HtmlA.style "background-color" "darkgrey"
    ]


textStdAttrs : List (Html.Attribute msg)
textStdAttrs =
    [ HtmlA.style "font-size" "20px"
    , HtmlA.style "font-family" "Consolas, monaco, monospace"
    , HtmlA.style "margin-left" "10px"
    , HtmlA.style "margin-right" "10px"
    ]

textLargeAttrs : List (Html.Attribute msg)
textLargeAttrs =
    [ HtmlA.style "font-size" "25px"
    , HtmlA.style "font-family" "Consolas, monaco, monospace"
    , HtmlA.style "margin-left" "10px"
    , HtmlA.style "margin-right" "10px"
    ]
