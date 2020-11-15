module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)

import Browser exposing (element)
import Browser.Events as E
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onClick)
import Http

import CodeGen.Spacemonkey as CSP
import Modules.Camera as Camera
import Modules.Show as Show
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

main = element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--------------------------------------------------------------------------------

init : Flags -> (Model, Cmd Msg)
init flags = let m = defaultModel flags
             in (m, initModel m.env)

defaultModel : Flags -> Model
defaultModel flags =
    { env = CSP.Dev
    , worldId = 0
    , world = { worldEnv = CSP.Dev
              , worldMaxX = 0
              , worldMaxY = 0 }
    , grid = []
    , users = []
    , viewOpts =
        Camera.initCam { windowWidth = flags.windowWidth
                       , windowHeight = flags.windowHeight
                       , camera = ((0, 0), (0, 0))
                       , cellSize = 40
                       }
    , errorMsg = Nothing }

initModel : CSP.Env -> Cmd Msg
initModel env = CSP.getWorldIdByEnv env GetWorldId

--------------------------------------------------------------------------------

view : Model -> Html msg
view = Show.show

--------------------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetWorldId (Ok mWid) -> case mWid of
            (Just wid) ->
                ({ model | worldId = wid }, getWorld wid)
            Nothing ->
                ({model | errorMsg = Just "World Id not found"}, Cmd.none)
        GetWorldId (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        GetWorld (Ok mWorld) -> case mWorld of
            (Just world) ->
                ({ model | world = world }, getGrid model.worldId)
            Nothing ->
                ({ model | errorMsg = Just "World ID not found"}, Cmd.none)
        GetWorld (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        GetGrid (Ok grid) ->
            ( { model | grid = grid }, getUsers model.worldId)
        GetGrid (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        GetUsers (Ok users) ->
            ( { model | users = users }, Cmd.none)
        GetUsers (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        WindowResize (w, h) ->
            let vo = Camera.reinitCam w h model.viewOpts
            in ({ model | viewOpts = vo }, Cmd.none)

getWorld : CSP.WorldId -> Cmd Msg
getWorld wid = CSP.getWorldByWid wid GetWorld

getGrid : CSP.WorldId -> Cmd Msg
getGrid wid = CSP.getGridByWorldid wid GetGrid

getUsers : CSP.WorldId -> Cmd Msg
getUsers wid = CSP.getUsersByWorldid wid GetUsers

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

--------------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onResize (\w h -> WindowResize (w, h))
