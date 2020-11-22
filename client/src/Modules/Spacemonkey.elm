
module Modules.Spacemonkey exposing (main)

import String exposing (fromInt)
import List.Extra as ListE

import Browser exposing (element)
import Browser.Events exposing (onResize)
import Keyboard as K

import Html exposing (Html, div)
import Json.Decode as Decode

import CodeGen.Spacemonkey as CSP
import Modules.Camera as Camera
import Modules.ShowGrid as ShowGrid
import Modules.ShowUI as ShowUI
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
    let c = Utils.getFacingM model
    in div
        []
        [ ShowGrid.show model
        , ShowUI.popup model.modalOpts c.cellCType model.userBuffer
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
        ClickCancelClosePopup ->
            cancelClosePopup model 
        ClickClosePopup ->
            closePopup model defaultModalOpts True
        ClickEnterTextEditMode ->
            let mo = model.modalOpts
                mo_ = { mo | textEditMode = True }
            in ({model | modalOpts = mo_}, Cmd.none)
        ClickExitTextEditMode ->
            let mo = model.modalOpts
                mo_ = { mo | textEditMode = False }
            in closePopup model mo_ False
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
            ({model | grid = applyColor model.grid model.self color}, Cmd.none)
        Recolor (Err e) ->
            Utils.httpError model e
        Retype (Ok ctype) ->
            ({model | grid = applyCType model.grid model.self ctype}, Cmd.none)
        Retype (Err e) ->
            Utils.httpError model e
        ApplyValue (Ok s) ->
            ({model | grid = applyValue model.grid model.self s}, Cmd.none)
        ApplyValue (Err e) ->
            Utils.httpError model e
        ClearValue (Ok b) ->
            ({model | grid = clearValue model.grid model.self b}, Cmd.none)
        ClearValue (Err e) ->
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
        True ->
            if validMove grid user dir
            then CSP.putMoveByUidByDirection uid dir Move
            else Cmd.none
        False ->
            CSP.putRefaceByUidByDirection uid dir Reface

validMove : Grid -> CSP.User -> CSP.Direction -> Bool
validMove grid user dir =
    let c = Utils.getFacing grid (user.userX, user.userY) dir
    in c.cellCType == CSP.Std && c.cellColor == CSP.White

applyMove : CSP.User -> CSP.Direction -> ViewOpts -> (CSP.User, ViewOpts)
applyMove u dir vo =
    let (dx, dy) = Utils.getDeltas dir
        (x, y) = (u.userX + dx, u.userY + dy)
        ((cx1, cy1), (cx2, cy2)) = vo.camera
        vo_ = if x <= cx1 || x >= cx2 - 1 || y <= cy1 || y >= cy2 - 1
              then Camera.moveCam dir vo
              else vo
    in ({ u | userX = x, userY = y }, vo_)

applyReface : CSP.User -> CSP.Direction -> CSP.User
applyReface u dir = { u | userFacing = dir }

cycleColor : CSP.WorldId -> Grid -> CSP.User -> Cmd Msg
cycleColor wid grid u =
    let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
    in case List.member c.cellCType [CSP.Std, CSP.Link, CSP.Text] of
           True -> let f = CSP.putCellColorByWidByXByYByColor
                       color = CSP.cycleColor c.cellColor
                   in f wid c.cellX c.cellY color Recolor
           False -> Cmd.none

applyColor : Grid -> CSP.User -> CSP.Color -> Grid
applyColor grid u color =
    let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
        c_ = {c | cellColor = color}
    in ListE.setIf (Utils.eqCoord c_) c_ grid

cycleCType : CSP.WorldId -> Grid -> CSP.User -> Cmd Msg
cycleCType wid grid u =
    let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
        f = CSP.putCellCTypeByWidByXByYByCelltype wid c.cellX c.cellY
    in case List.member c.cellCType [CSP.Std, CSP.Link, CSP.Text] of
           True -> case c.cellCType of
                       CSP.Std -> f CSP.Link Retype
                       CSP.Link -> f CSP.Text Retype
                       _ -> f CSP.Std Retype
           _ -> Cmd.none

applyCType : Grid -> CSP.User -> CSP.CellType -> Grid
applyCType grid u ctype =
    let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
        c_ = {c | cellCType = ctype}
    in ListE.setIf (Utils.eqCoord c_) c_ grid

applyValue : Grid -> CSP.User -> String -> Grid
applyValue grid u s =
    let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
        c_ = {c | cellValue = s}
    in ListE.setIf (Utils.eqCoord c_) c_ grid

clearValue : Grid -> CSP.User -> Bool -> Grid
clearValue grid u ok =
    case ok of
        False -> grid
        True ->
            let c = Utils.getFacing grid (u.userX, u.userY) u.userFacing
                c_ = {c | cellValue = ""}
            in ListE.setIf (Utils.eqCoord c_) c_ grid

--------------------------------------------------------------------------------

keyHandler : Model -> List K.Key -> (Model, Cmd Msg)
keyHandler model keys =
    case keys of
        [] ->
            ({ model | pressedKeys = [] }, Cmd.none)
        (k::ks) ->
            case model.modalOpts.popupOpen of
                True -> popupKeyHandler model k ks
                False -> normalKeyHandler model k ks

popupKeyHandler : Model -> K.Key -> List K.Key -> (Model, Cmd Msg)
popupKeyHandler model k ks =
    case k of
        K.Escape ->
            cancelClosePopup model
        K.Enter ->
            closePopup model defaultModalOpts True
        _ ->
            (model, Cmd.none)

cancelClosePopup : Model -> (Model, Cmd Msg)
cancelClosePopup model =
    ({ model | modalOpts = defaultModalOpts, userBuffer = "" }, Cmd.none)

closePopup : Model -> ModalOpts -> Bool -> (Model, Cmd Msg)
closePopup model mo clearBuffer =
    let c = Utils.getFacingM model
        wid = model.worldId
        s = model.userBuffer
        put = CSP.putCellValueByWidByXByY
        clear = CSP.putClearCellByWidByXByY
        buffer = if clearBuffer then "" else model.userBuffer
        cmd = if s /= "" then put wid c.cellX c.cellY s ApplyValue
              else clear wid c.cellX c.cellY ClearValue
    in ({ model | modalOpts = mo, userBuffer = buffer }, cmd)

normalKeyHandler : Model -> K.Key -> List K.Key -> (Model, Cmd Msg)
normalKeyHandler model k ks =
    let mv = moveOrReface model.grid model.userId model.self
        cycleC = cycleColor model.worldId model.grid model.self
        cycleT = cycleCType model.worldId model.grid model.self
        model_ =
            case k of
                K.Character "E" ->
                    let c = (Utils.getFacingM model)
                        mo = model.modalOpts
                        mo_ = { mo | popupOpen = True }
                    in case canEditValue c of
                           True -> { model | modalOpts = mo_
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
                K.ArrowLeft -> mv CSP.West
                K.Character "C" -> cycleC
                K.Character "T" -> cycleT
                _ -> Cmd.none
    in (model_, cmd)

canEditValue : CSP.Cell -> Bool
canEditValue c =
    (c.cellCType == CSP.Std && c.cellColor /= CSP.White) ||
    List.member c.cellCType [CSP.Link, CSP.Text]

--------------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResize (w, h))
        , Sub.map KeyMsg K.subscriptions
        ]
