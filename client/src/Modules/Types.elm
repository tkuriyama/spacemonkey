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
    = GetWorldId (Result Http.Error (Maybe CSP.WorldId))
    | GetWorld (Result Http.Error (Maybe CSP.World))
    | GetGrid (Result Http.Error (List CSP.Cell))
    | GetUsers (Result Http.Error (List CSP.User))
    | Move (Result Http.Error (CSP.Direction))
    | Reface (Result Http.Error (CSP.Direction))
    | DirectionKeyPress CSP.Direction
    | NoAction
    | ToggleColor
    | WindowResize (Int, Int)

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

getDefaultModel : Model
getDefaultModel =
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
