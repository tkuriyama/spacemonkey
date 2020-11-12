module Modules.Types exposing (..)

import Http exposing (Error)

import CodeGen.Spacemonkey as CSP

type Msg
    = GetWorldId (Result Http.Error (Maybe CSP.WorldId))
    | GetGrid (Result Http.Error Grid)

type alias Model
    = { env : CSP.Env
      , worldId : CSP.WorldId
      , world : CSP.World
      , grid : Grid
      , statusMsg : Maybe String
      , errorMsg : Maybe String
      }

type alias Grid = List CSP.Cell
