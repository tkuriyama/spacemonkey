module Modules.Types exposing (..)

import List.Extra as ListE
import Maybe.Extra as MaybeE

import Http exposing (Error)

import CodeGen.Spacemonkey as CSP

type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    }

type Msg
    = WindowResize (Int, Int)
    | DirectionKeyPress CSP.Direction
    | CycleColorKeyPress
    | NoAction
    | GetWorldId (Result Http.Error (Maybe CSP.WorldId))
    | GetWorld (Result Http.Error (Maybe CSP.World))
    | GetGrid (Result Http.Error (List CSP.Cell))
    | GetUser (Result Http.Error (Maybe CSP.User))
    | GetUsers (Result Http.Error (List CSP.User))
    | Move (Result Http.Error (CSP.Direction))
    | Reface (Result Http.Error (CSP.Direction))
    | Recolor (Result Http.Error (CSP.Color))

type alias Model
    = { env : CSP.Env
      , worldId : CSP.WorldId
      , world : CSP.World
      , grid : Grid
      , userId : CSP.UserId
      , self : CSP.User
      , others : List CSP.User
      , viewOpts : ViewOpts
      , errorMsg : Maybe String
      }

type alias Grid = List CSP.Cell

type alias Point = (Int, Int)

type alias ViewOpts
    = { windowWidth : Int
      , windowHeight : Int
      , cellSize : Int
      , camera : (Point, Point)
      }

--------------------------------------------------------------------------------

defaultModel : Model
defaultModel =
    { env = CSP.Dev -- TODO initialize with login
    , worldId = 0
    , world = { worldEnv = CSP.Dev
              , worldMaxX = 0
              , worldMaxY = 0 }
    , grid = []
    , userId = 1 -- TODO intialize with login
    , self = { userEnv = 0
             , userName = ""
             , userX = 0
             , userY = 0
             , userFacing = CSP.East
             }
    , others = []
    , viewOpts = { windowWidth = 0
                 , windowHeight = 0
                 , camera = ((0, 0), (0, 0))
                 , cellSize = 40
                 }
    , errorMsg = Nothing
    }

defaultCell : CSP.Cell
defaultCell =
    { cellEnv = 1
    , cellX = 0
    , cellY = 0
    , cellColor = CSP.White
    , cellValue = ""
    , cellCType = CSP.Std
    }
