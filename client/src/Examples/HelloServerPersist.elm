module Modules.HelloServerPersist exposing (main)

import String exposing (fromInt)

import Http
import Browser exposing (element)
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onClick)

import CodeGen.HelloServerPersist as HSP

type Msg = SendHttpRequest
         | DataReceived (Result Http.Error (Maybe HSP.ServerState))
         | UpdateState
         | IncrementCtr

type alias Model = { serverState : HSP.ServerState
                   , errorMsg : Maybe String }

initModel : Model
initModel = { serverState = HSP.ServerState "" 0
            , errorMsg = Nothing }

init : () -> (Model, Cmd Msg)
init _ = (initModel, getModel)

view : Model -> Html Msg
view m =
    div []
        [ text ("Server State String: " ++ m.serverState.serverStateState )
        , br [] []
        , text ("Server State Counter: " ++
                fromInt m.serverState.serverStateCounter ++ "    ")
        , button [ onClick IncrementCtr ] [ text "+1" ]
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            (model, getModel)
        UpdateState ->
            (model, Cmd.none)
        IncrementCtr -> (model, incCounter)
        DataReceived (Ok s) -> case s of
            (Just st) -> ({ model | serverState = st }, Cmd.none)
            Nothing -> (model, Cmd.none)
        DataReceived (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMessage httpError) }
            , Cmd.none)

getModel : Cmd Msg
getModel = HSP.getRequest DataReceived

incCounter : Cmd Msg
incCounter = HSP.putIncrement DataReceived

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message
        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."
        Http.NetworkError ->
            "Unable to reach server."
        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode
        Http.BadBody message ->
            message

main = element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
