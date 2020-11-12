module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)

import Http
import Browser exposing (element)
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onClick)

import CodeGen.Spacemonkey as CSP
import Show as Show

type Msg
    = GetWorldId (Result Http.Error (Maybe CSP.WorldId))
    | GetGrid (Result Http.Error CSP.Grid)

type alias Model = { env : CSP.Env
                   , worldId : CSP.WorldId
                   , world : CSP.World
                   , grid : List CSP.Cell
                   , statusMsg : Maybe String
                   , errorMsg : Maybe String
                   }

--------------------------------------------------------------------------------


main = element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

--------------------------------------------------------------------------------

init : () -> (Model, Cmd Msg)
init _ = let m = defaultModel
         in (m, initModel m.env)

defaultModel : Model
defaultModel = { env = CSP.Dev
               , worldId = 0
               , world = { worldEnv = CSP.Dev
                         , worldMaxX = 0
                         , worldMaxY = 0 }
               , grid = []
               , statusMsg = Nothing
               , errorMsg = Nothing }

initModel : CSP.Env -> Cmd Msg
initModel env = CSP.getWorldIdByEnv env GetWorldId

--------------------------------------------------------------------------------

view : Model -> Html Msg
view m = Show.show m

--------------------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetWorldId (Ok mWid) -> case mWid of
            (Just wid) -> ({ model | worldId = wid }, getGrid wid)
            Nothing -> (model, Cmd.none)
        GetWorldId (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        GetGrid (Ok grid) ->
            ({ model | grid = grid }, Cmd.none)
        GetGrid (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)


getGrid : CSP.WorldId -> Cmd Msg
getGrid wid = CSP.getGridByWorldid wid GetGrid

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
