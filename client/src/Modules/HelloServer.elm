module Modules.HelloServer exposing (main)
    
import String exposing (fromInt)

import Http
import Browser exposing (element)
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onClick)

import CodeGen.HelloServer as HS

type Msg = SendHttpRequest
         | DataReceived (Result Http.Error (HS.ServerState))
         | UpdateState
         | IncrementCtr
           
type alias Model = { serverState : HS.ServerState
                   , localState : String
                   , errorMsg : Maybe String }

initModel : Model
initModel = { serverState = HS.ServerState "" 0
            , localState = ""
            , errorMsg = Nothing }

init : () -> (Model, Cmd Msg)
init _ = (initModel, getModel)
             
view : Model -> Html Msg
view m =
    div []
        [ text ("Server State String: " ++ m.serverState.state )
        , br [] []
        , text ("Server State Counter: " ++ fromInt m.serverState.counter ++ "    ")
        , button [ onClick IncrementCtr ] [ text "+1" ]
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            (model, getModel)
        UpdateState ->
            (model, Cmd.none)
        IncrementCtr ->
            let n = model.serverState.counter + 1
            in (model, setCounter n)
        DataReceived (Ok s) ->
            ( { model | serverState = s }, Cmd.none)
        DataReceived (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMessage httpError) }
            , Cmd.none)

getModel : Cmd Msg
getModel = HS.getServer DataReceived
            
setCounter : Int -> Cmd Msg
setCounter n = HS.putSetCounterByCounter n DataReceived
            
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
