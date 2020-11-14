module Modules.Types exposing (..)

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
    | WindowResize (Int, Int)

type alias Model
    = { windowWidth: Int
      , windowHeight: Int
      , env : CSP.Env
      , worldId : CSP.WorldId
      , world : CSP.World
      , grid : Grid
      , statusMsg : Maybe String
      , errorMsg : Maybe String
      }

type alias Grid = List CSP.Cell
