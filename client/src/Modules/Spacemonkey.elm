
module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)
import List.Extra as ListE
import Update.Extra as UpdateE

import Browser exposing (element)
import Browser.Events as E
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
import Http
import Json.Decode as Decode

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
    , userId = 1
    , userName = "🦉"  -- TODO: don't hard code this
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
        DirectionKeyPress dir ->
            let cmd = moveOrReface model.users model.userId model.userName dir
            in (model, cmd)
        Move (Ok dir) ->
            let users = applyMove model.users model.userName dir
            in ({model | users = users}, Cmd.none)
        Move (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        Reface (Ok dir) ->
            let users = applyReface model.users model.userName dir
            in ({model | users = users}, Cmd.none)
        Reface (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        NoAction ->
            (model, Cmd.none)

getWorld : CSP.WorldId -> Cmd Msg
getWorld wid = CSP.getWorldByWid wid GetWorld

getGrid : CSP.WorldId -> Cmd Msg
getGrid wid = CSP.getGridByWorldid wid GetGrid

getUsers : CSP.WorldId -> Cmd Msg
getUsers wid = CSP.getUsersByWorldid wid GetUsers

moveOrReface : List CSP.User -> CSP.UserId -> String -> CSP.Direction ->
               Cmd Msg
moveOrReface users uid name dir =
    case ListE.find (\u -> u.userName == name) users of
        Nothing -> Cmd.none
        Just u_ -> case u_.userFacing == dir of
                            True -> putMove uid dir    --TODO check for validity
                            False -> putReface uid dir

putMove : CSP.UserId -> CSP.Direction -> Cmd Msg
putMove uid dir = CSP.putMoveByUserIdByDirection uid dir Move

putReface : CSP.UserId -> CSP.Direction -> Cmd Msg
putReface uid dir = CSP.putRefaceByUserIdByDirection uid dir Reface

applyMove : List CSP.User -> String -> CSP.Direction -> List CSP.User
applyMove users name dir =
    -- TODO check for camera move
    let (dx, dy) = case dir of
                       CSP.North -> (0, -1)
                       CSP.South -> (0, 1)
                       CSP.East -> (1, 0)
                       CSP.West -> (-1, 0)
        f u = { u | userX = u.userX + dx, userY = u.userY + dy }
    in ListE.updateIf (\u ->  u.userName == name) f users

applyReface : List CSP.User -> String -> CSP.Direction -> List CSP.User
applyReface users name dir =
    ListE.updateIf
        (\u -> u.userName == name)
        (\u -> { u | userFacing = dir })
        users

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
    Sub.batch
        [ E.onResize (\w h -> WindowResize (w, h))
        , E.onKeyPress keyDecoder
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey keyValue =
    case keyValue of
        "w" -> DirectionKeyPress CSP.North
        "s" -> DirectionKeyPress CSP.South
        "d" -> DirectionKeyPress CSP.East
        "a" -> DirectionKeyPress CSP.West
        _ -> NoAction
