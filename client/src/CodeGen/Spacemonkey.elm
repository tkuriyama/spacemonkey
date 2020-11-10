module CodeGen.Spacemonkey exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias World  =
   { worldEnv: Environment
   , worldMaxX: Int
   , worldMaxY: Int
   }

jsonDecWorld : Json.Decode.Decoder ( World )
jsonDecWorld =
   Json.Decode.succeed (\pworldEnv pworldMaxX pworldMaxY -> {worldEnv = pworldEnv, worldMaxX = pworldMaxX, worldMaxY = pworldMaxY})
   |> required "worldEnv" (jsonDecEnvironment)
   |> required "worldMaxX" (Json.Decode.int)
   |> required "worldMaxY" (Json.Decode.int)

jsonEncWorld : World -> Value
jsonEncWorld  val =
   Json.Encode.object
   [ ("worldEnv", jsonEncEnvironment val.worldEnv)
   , ("worldMaxX", Json.Encode.int val.worldMaxX)
   , ("worldMaxY", Json.Encode.int val.worldMaxY)
   ]



type alias Grid  =
   { gridEnv: (Key World)
   , gridX: Int
   , gridY: Int
   , gridCellColor: Color
   , gridCellType: CellType
   , gridCellValue: String
   , gridCellUser: (Maybe (Key User))
   }

jsonDecGrid : Json.Decode.Decoder ( Grid )
jsonDecGrid =
   Json.Decode.succeed (\pgridEnv pgridX pgridY pgridCellColor pgridCellType pgridCellValue pgridCellUser -> {gridEnv = pgridEnv, gridX = pgridX, gridY = pgridY, gridCellColor = pgridCellColor, gridCellType = pgridCellType, gridCellValue = pgridCellValue, gridCellUser = pgridCellUser})
   |> required "gridEnv" (jsonDecKey (jsonDecWorld))
   |> required "gridX" (Json.Decode.int)
   |> required "gridY" (Json.Decode.int)
   |> required "gridCellColor" (jsonDecColor)
   |> required "gridCellType" (jsonDecCellType)
   |> required "gridCellValue" (Json.Decode.string)
   |> fnullable "gridCellUser" (jsonDecKey (jsonDecUser))

jsonEncGrid : Grid -> Value
jsonEncGrid  val =
   Json.Encode.object
   [ ("gridEnv", (jsonEncKey (jsonEncWorld)) val.gridEnv)
   , ("gridX", Json.Encode.int val.gridX)
   , ("gridY", Json.Encode.int val.gridY)
   , ("gridCellColor", jsonEncColor val.gridCellColor)
   , ("gridCellType", jsonEncCellType val.gridCellType)
   , ("gridCellValue", Json.Encode.string val.gridCellValue)
   , ("gridCellUser", (maybeEncode ((jsonEncKey (jsonEncUser)))) val.gridCellUser)
   ]



type alias Message  =
   { messageEnv: (Key World)
   , messageValue: String
   }

jsonDecMessage : Json.Decode.Decoder ( Message )
jsonDecMessage =
   Json.Decode.succeed (\pmessageEnv pmessageValue -> {messageEnv = pmessageEnv, messageValue = pmessageValue})
   |> required "messageEnv" (jsonDecKey (jsonDecWorld))
   |> required "messageValue" (Json.Decode.string)

jsonEncMessage : Message -> Value
jsonEncMessage  val =
   Json.Encode.object
   [ ("messageEnv", (jsonEncKey (jsonEncWorld)) val.messageEnv)
   , ("messageValue", Json.Encode.string val.messageValue)
   ]



type alias User  =
   { userEnv: (Key World)
   , userName: String
   , userLoc: (Key Grid)
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\puserEnv puserName puserLoc -> {userEnv = puserEnv, userName = puserName, userLoc = puserLoc})
   |> required "userEnv" (jsonDecKey (jsonDecWorld))
   |> required "userName" (Json.Decode.string)
   |> required "userLoc" (jsonDecKey (jsonDecGrid))

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("userEnv", (jsonEncKey (jsonEncWorld)) val.userEnv)
   , ("userName", Json.Encode.string val.userName)
   , ("userLoc", (jsonEncKey (jsonEncGrid)) val.userLoc)
   ]



type Environment  =
    Dev 
    | Prod 

jsonDecEnvironment : Json.Decode.Decoder ( Environment )
jsonDecEnvironment = 
    let jsonDecDictEnvironment = Dict.fromList [("Dev", Dev), ("Prod", Prod)]
    in  decodeSumUnaries "Environment" jsonDecDictEnvironment

jsonEncEnvironment : Environment -> Value
jsonEncEnvironment  val =
    case val of
        Dev -> Json.Encode.string "Dev"
        Prod -> Json.Encode.string "Prod"



type Color  =
    White 
    | Yellow 
    | Red 
    | Green 
    | Blue 
    | Grey 

jsonDecColor : Json.Decode.Decoder ( Color )
jsonDecColor = 
    let jsonDecDictColor = Dict.fromList [("White", White), ("Yellow", Yellow), ("Red", Red), ("Green", Green), ("Blue", Blue), ("Grey", Grey)]
    in  decodeSumUnaries "Color" jsonDecDictColor

jsonEncColor : Color -> Value
jsonEncColor  val =
    case val of
        White -> Json.Encode.string "White"
        Yellow -> Json.Encode.string "Yellow"
        Red -> Json.Encode.string "Red"
        Green -> Json.Encode.string "Green"
        Blue -> Json.Encode.string "Blue"
        Grey -> Json.Encode.string "Grey"



type CellType  =
    Std 
    | Link 
    | Text 
    | Fixed 

jsonDecCellType : Json.Decode.Decoder ( CellType )
jsonDecCellType = 
    let jsonDecDictCellType = Dict.fromList [("Std", Std), ("Link", Link), ("Text", Text), ("Fixed", Fixed)]
    in  decodeSumUnaries "CellType" jsonDecDictCellType

jsonEncCellType : CellType -> Value
jsonEncCellType  val =
    case val of
        Std -> Json.Encode.string "Std"
        Link -> Json.Encode.string "Link"
        Text -> Json.Encode.string "Text"
        Fixed -> Json.Encode.string "Fixed"


getGetWorldByEnv : String -> (Result Http.Error  ((Maybe World))  -> msg) -> Cmd msg
getGetWorldByEnv capture_env toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "getWorld"
                    , (capture_env)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecWorld))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGetGridByWorldid : (Key World) -> (Result Http.Error  ((List Grid))  -> msg) -> Cmd msg
getGetGridByWorldid capture_worldid toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "getGrid"
                    , (capture_worldid |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecGrid))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGetMsgsByWorldidByRecentN : (Key World) -> Int -> (Result Http.Error  ((List Message))  -> msg) -> Cmd msg
getGetMsgsByWorldidByRecentN capture_worldid capture_recentN toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "getMsgs"
                    , (capture_worldid |> String.fromInt)
                    , (capture_recentN |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecMessage))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGetUsersByWorldid : (Key World) -> (Result Http.Error  ((List User))  -> msg) -> Cmd msg
getGetUsersByWorldid capture_worldid toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "getUsers"
                    , (capture_worldid |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecUser))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putSetGridColorByWorldidByXByYByC : (Key World) -> Int -> Int -> Color -> (Result Http.Error  (Color)  -> msg) -> Cmd msg
putSetGridColorByWorldidByXByYByC capture_worldid capture_x capture_y capture_c toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "setGridColor"
                    , (capture_worldid |> String.fromInt)
                    , (capture_x |> String.fromInt)
                    , (capture_y |> String.fromInt)
                    , (capture_c |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecColor
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
