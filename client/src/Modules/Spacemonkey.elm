
module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)
import List.Extra as ListE
import Update.Extra as UpdateE

import Browser exposing (element)
import Browser.Events exposing (onResize)
import Keyboard as K

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
    let m = defaultModel
        vo = m.viewOpts
        vo_ = { vo | windowWidth = flags.windowWidth,
                     windowHeight = flags.windowHeight
              }
    in { m | viewOpts = Camera.initCam vo_ }

--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
    div
    []
    [ Show.show model
    , Show.popup model.popupOpen model.userBuffer
    ]


--------------------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WindowResize (w, h) ->
            let vo = Camera.reinitCam w h model.viewOpts
            in ({ model | viewOpts = vo }, Cmd.none)
        KeyMsg keyMsg ->
            keyHandler model <| K.update keyMsg model.pressedKeys
        UpdateUserBuffer s ->
            ({ model | userBuffer = s }, Cmd.none)
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
        Recolor (Ok color) ->
            let grid = applyColor model.grid model.self color
            in ({model | grid = grid}, Cmd.none)
        Recolor (Err e) ->
            Utils.httpError model e
        ApplyValue (Ok s) ->
            let grid = applyValue model.grid model.self s
            in ({model | grid = grid}, Cmd.none)
        ApplyValue (Err e) ->
            Utils.httpError model e

initWorld : CSP.Env -> Cmd Msg
initWorld env = CSP.getWorldIdByEnv env GetWorldId

getWorld : CSP.WorldId -> Cmd Msg
getWorld wid = CSP.getWorldByWid wid GetWorld

getUser : CSP.UserId -> Cmd Msg
getUser uid = CSP.getUserByUid uid GetUser

getGrid : CSP.WorldId -> Cmd Msg
getGrid wid = CSP.getGridByWid wid GetGrid

getUsers : CSP.WorldId -> Cmd Msg
getUsers wid = CSP.getUsersByWid wid GetUsers

moveOrReface : Grid -> CSP.UserId -> CSP.User -> CSP.Direction -> Cmd Msg
moveOrReface grid uid user dir =
    case user.userFacing == dir of
        True -> if validMove grid user dir then putMove uid dir else Cmd.none
        False -> putReface uid dir

validMove : Grid -> CSP.User -> CSP.Direction -> Bool
validMove grid user dir =
    let c = Utils.getFacing grid (user.userX, user.userY) dir
    in c.cellCType == CSP.Std && c.cellColor == CSP.White

putMove : CSP.UserId -> CSP.Direction -> Cmd Msg
putMove uid dir = CSP.putMoveByUidByDirection uid dir Move

putReface : CSP.UserId -> CSP.Direction -> Cmd Msg
putReface uid dir = CSP.putRefaceByUidByDirection uid dir Reface

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

cycleColor : CSP.WorldId -> Grid -> CSP.User -> Cmd Msg
cycleColor wid grid user =
    let c = Utils.getFacing grid (user.userX, user.userY) user.userFacing
    in case c.cellCType of
           CSP.Std -> let f = CSP.putCellColorByWidByXByYByColor
                          color = CSP.cycleColor c.cellColor
                      in f wid c.cellX c.cellY color Recolor
           _ -> Cmd.none

applyColor : Grid -> CSP.User -> CSP.Color -> Grid
applyColor grid user color =
    let c = Utils.getFacing grid (user.userX, user.userY) user.userFacing
        c_ = {c | cellColor = color}
    in ListE.setIf (Utils.eqCoord c_) c_ grid

saveCellValue : CSP.WorldId -> CSP.Cell -> String -> Cmd Msg
saveCellValue wid c s =
    CSP.putCellValueByWidByXByYByVal wid c.cellX c.cellY s ApplyValue

applyValue : Grid -> CSP.User -> String -> Grid
applyValue grid user s =
    let c = Utils.getFacing grid (user.userX, user.userY) user.userFacing
        c_ = {c | cellValue = s}
    in ListE.setIf (Utils.eqCoord c_) c_ grid

--------------------------------------------------------------------------------

keyHandler : Model -> List K.Key -> (Model, Cmd Msg)
keyHandler model keys =
    case keys of
        [] -> ({ model | pressedKeys = [] }, Cmd.none)
        (k::ks) -> case model.popupOpen of
                       True -> popupKeyHandler model k ks
                       False -> normalKeyHandler model k ks

popupKeyHandler : Model -> K.Key -> List K.Key -> (Model, Cmd Msg)
popupKeyHandler model k ks =
    case k of
        K.Escape ->
            ({ model | popupOpen = False, userBuffer = ""}, Cmd.none)
        K.Enter ->
            let c = Utils.getFacingM model
                wid = model.worldId
                s = model.userBuffer
            in ( { model | popupOpen = False, userBuffer = ""}
               , saveCellValue wid c s )
        _ -> (model, Cmd.none)

normalKeyHandler : Model -> K.Key -> List K.Key -> (Model, Cmd Msg)
normalKeyHandler model k ks =
    let mv = moveOrReface model.grid model.userId model.self
        cycle = cycleColor model.worldId model.grid model.self
        model_ =
            case k of
                K.Character "E" ->
                    let c = Utils.getFacingM model
                    in case canEditText c of
                           True -> { model | popupOpen = True
                                   , userBuffer = c.cellValue }
                           False -> model
                _ -> model
        cmd =
            case k of
                K.Character "W" -> mv CSP.North
                K.ArrowUp -> mv CSP.North
                K.Character "S" -> mv CSP.South
                K.ArrowDown -> mv CSP.South
                K.Character "D" -> mv CSP.East
                K.ArrowRight -> mv CSP.East
                K.Character "A" -> mv CSP.West
                K.Character "C" -> cycle
                K.ArrowLeft -> mv CSP.West
                _ -> Cmd.none
    in (model_, cmd)

canEditText : CSP.Cell -> Bool
canEditText c = c.cellCType == CSP.Std && c.cellColor /= CSP.White

canEditType : CSP.Cell -> Bool
canEditType c = c.cellCType /= CSP.Fixed

--------------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize (w, h))
        , Sub.map KeyMsg K.subscriptions
        ]
