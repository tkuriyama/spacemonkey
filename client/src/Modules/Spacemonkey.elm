module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)

import Http
import Browser exposing (element)
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onClick)

import CodeGen.Spacemonkey as CSP

type Msg = GetWorldId (Result Http.Error (Maybe (CSP.Key CSP.World)))

type alias Model = { env : CSP.Environment
                   , worldId : CSP.Key CSP.World
                   , world : CSP.World
                   , grid : List CSP.CEll
                   , statusMsg : Maybe String
                   , errorMsg : Maybe String
                   }

--------------------------------------------------------------------------------

init : () -> (Model, Cmd Msg)
init _ = (defaultModel, initModel)

defaultModel : Model
defaultModel = { env = CSP.Dev
               , worldId = 0
               , world = { worldEnv = CSP.Dev
                         , worldX = 0
                         , worldY = 0 }
               , grid = []
               , statusMsg = Nothing
               , errorMsg = Nothing }

initModel : Cmd.Msg
initModel = HSP.getGetWorldIdByEnv DataReceived

view : Model -> Html Msg
view m =
    div []
        []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
--        SendHttpRequest ->
--            (model, getModel)
        GetWorldId (Ok mWid) -> case mWid of
            (Just wid) -> ({ model | worldId = wid }, Cmd.none)
            Nothing -> (model, Cmd.none)
        GetWorldId (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)

buildErrorMsg : Http.Error -> String
buildErrorMsg httpError =
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
