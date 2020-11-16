
module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)
import List.Extra as ListE
import Update.Extra as UpdateE

import Browser exposing (element)
import Browser.Events as E
import Html exposing (Html, div, text, button, input, br)
import Html.Attributes exposing (class, value, placeholder)
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
        vo = m.viewOpts
        vo_ = { vo | windowWidth = flags.windowWidth,
                     windowHeight = flags.windowHeight
              }
    in { m | viewOpts = Camera.initCam vo_ }

--------------------------------------------------------------------------------

view : Model -> Html msg
view = Show.show

--------------------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WindowResize (w, h) ->
            let vo = Camera.reinitCam w h model.viewOpts
            in ({ model | viewOpts = vo }, Cmd.none)
        DirectionKeyPress dir ->
            (model, moveOrReface model.grid model.userId model.self dir)
        ToggleColorKeyPress ->
            (model, Cmd.none)
        NoAction ->
            (model, Cmd.none)
        -- HTTP Rest Updates
        GetWorldId (Ok mWid) -> case mWid of
            (Just wid) ->
                ({ model | worldId = wid }, getWorld wid)
            Nothing ->
                ({model | errorMsg = Just "World Id not found"}, Cmd.none)
        GetWorldId (Err e) ->
            Utils.httpError model e
        GetWorld (Ok mWorld) -> case mWorld of
            (Just world) ->
                ({ model | world = world }, getUser model.userId)
            Nothing ->
                ({ model | errorMsg = Just "World not found"}, Cmd.none)
        GetWorld (Err e) ->
            Utils.httpError model e
        GetUser (Ok mUser) -> case mUser of
            (Just user) ->
                ({ model | self = user }, getGrid model.worldId)
            Nothing ->
                ({ model | errorMsg = Just "User not found"}, Cmd.none)
        GetUser (Err e) ->
            Utils.httpError model e
        GetGrid (Ok grid) ->
            ({ model | grid = grid }, getUsers model.worldId)
        GetGrid (Err e) ->
            Utils.httpError model e
        GetUsers (Ok users) ->
            let name = model.self.userName
                users_ = List.filter (\u -> u.userName /= name) users
            in ({ model | others = users_ }, Cmd.none)
        GetUsers (Err e) ->
            Utils.httpError model e
        Move (Ok dir) ->
            let (user, vo) = applyMove model.self dir model.viewOpts
            in ({model | self = user, viewOpts = vo}, Cmd.none)
        Move (Err e) ->
            Utils.httpError model e
        Reface (Ok dir) ->
            ({model | self = applyReface model.self dir}, Cmd.none)
        Reface(Err e) ->
            Utils.httpError model e

initWorld : CSP.Env -> Cmd Msg
initWorld env = CSP.getWorldIdByEnv env GetWorldId

getWorld : CSP.WorldId -> Cmd Msg
getWorld wid = CSP.getWorldByWid wid GetWorld

getUser : CSP.UserId -> Cmd Msg
getUser uid = CSP.getUserByUserid uid GetUser

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
validMove grid user dir =
    case Utils.getFacing grid (user.userX, user.userY) dir of
        Nothing -> False
        (Just c) -> c.cellCType == CSP.Std && c.cellColor == CSP.White

putMove : CSP.UserId -> CSP.Direction -> Cmd Msg
putMove uid dir = CSP.putMoveByUserIdByDirection uid dir Move

putReface : CSP.UserId -> CSP.Direction -> Cmd Msg
putReface uid dir = CSP.putRefaceByUserIdByDirection uid dir Reface

applyMove : CSP.User -> CSP.Direction -> ViewOpts -> (CSP.User, ViewOpts)
applyMove user dir vo =
    let (dx, dy) = Utils.getDeltas dir
        (x, y) = (user.userX + dx, user.userY + dy)
        ((cx1, cy1), (cx2, cy2)) = vo.camera
        vo_ = if x <= cx1 || x >= cx2 - 1 || y <= cy1 || y >= cy2 - 1
              then Camera.moveCam dir vo
              else vo
    in ({ user | userX = x, userY = y }, vo_)

applyReface : CSP.User -> CSP.Direction -> CSP.User
applyReface user dir = { user | userFacing = dir }

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
        "c" -> ToggleColorKeyPress
        _ -> NoAction
