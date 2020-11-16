
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
import Modules.Utils as Utils

--------------------------------------------------------------------------------

main = element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--------------------------------------------------------------------------------

init : Flags -> (Model, Cmd Msg)
init flags = let m = initModel flags
             in (m, initWorld m.env)

initModel : Flags -> Model
initModel flags =
    let m = getDefaultModel
    in { m | viewOpts = Camera.initCam m.viewOpts }

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
            ( { model | others = users }, Cmd.none)
        GetUsers (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        WindowResize (w, h) ->
            let vo = Camera.reinitCam w h model.viewOpts
            in ({ model | viewOpts = vo }, Cmd.none)
        DirectionKeyPress dir ->
            (model, moveOrReface model.grid model.userId model.self dir)
        ToggleColor ->
            (model, Cmd.none)
        Move (Ok dir) ->
            ({model | self = applyMove model.self dir}, Cmd.none)
        Move (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        Reface (Ok dir) ->
            ({model | self = applyReface model.self dir}, Cmd.none)
        Reface (Err httpError) ->
            ({ model | errorMsg = Just (buildErrorMsg httpError) }, Cmd.none)
        NoAction ->
            (model, Cmd.none)

initWorld : CSP.Env -> Cmd Msg
initWorld env = CSP.getWorldIdByEnv env GetWorldId

getWorld : CSP.WorldId -> Cmd Msg
getWorld wid = CSP.getWorldByWid wid GetWorld

getGrid : CSP.WorldId -> Cmd Msg
getGrid wid = CSP.getGridByWorldid wid GetGrid

getUsers : CSP.WorldId -> Cmd Msg
getUsers wid = CSP.getUsersByWorldid wid GetUsers

moveOrReface : Grid -> CSP.UserId -> CSP.User -> CSP.Direction -> Cmd Msg
moveOrReface grid uid user dir =
    case user.userFacing == dir of
        True -> if validMove grid user dir then putMove uid dir else Cmd.none
        False -> putReface uid dir

validMove : Grid -> CSP.User -> CSP.Direction -> Bool
validMove grid user dir = True

putMove : CSP.UserId -> CSP.Direction -> Cmd Msg
putMove uid dir = CSP.putMoveByUserIdByDirection uid dir Move

putReface : CSP.UserId -> CSP.Direction -> Cmd Msg
putReface uid dir = CSP.putRefaceByUserIdByDirection uid dir Reface

applyMove : CSP.User -> CSP.Direction -> CSP.User
applyMove user dir =
    -- TODO check for camera move
    let (dx, dy) = Utils.moveDeltas dir
    in { user | userX = user.userX + dx, userY = user.userY + dy }

applyReface : CSP.User -> CSP.Direction -> CSP.User
applyReface user dir = { user | userFacing = dir }

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
        "c" -> ToggleColor
        _ -> NoAction
