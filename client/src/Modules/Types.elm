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
    | WindowResize (Int, Int)
    | DirectionKeyPress CSP.Direction
    | Move (Result Http.Error (CSP.Direction))
    | Reface (Result Http.Error (CSP.Direction))
    | NoAction

type alias Model
    = { env : CSP.Env
      , worldId : CSP.WorldId
      , world : CSP.World
      , grid : Grid
      , userId : CSP.UserId
      , userName : String
      , users : List CSP.User
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
-- Type conversion helpers

-- listToGrid : Int -> List CSP.Cell -> Grid
-- listToGrid n cs = cs
--                 |> List.sortBy (\c -> c.cellY)
--                 |> genGroups n []
--                 |> List.map (List.sortBy (\c -> c.cellX))
--                 |> List.map (Z.fromList)
--                 |> MaybeE.combine
--                 |> MaybeE.unwrap Empty (\zs -> case Z.fromList zs of
--                                                    Nothing -> Empty
--                                                    Just z -> Grid z)

-- genGroups : Int -> List (List a) -> List a -> List (List a)
-- genGroups n acc xs =
--     case xs of
--         [] -> List.reverse acc
--         xs_ -> let row = List.take n xs
--                    rest = List.drop n xs_
--                in genGroups n (row :: acc) rest

