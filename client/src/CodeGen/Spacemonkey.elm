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
   { worldEnv: Env
   , worldMaxX: Int
   , worldMaxY: Int
   }

jsonDecWorld : Json.Decode.Decoder ( World )
jsonDecWorld =
   Json.Decode.succeed (\pworldEnv pworldMaxX pworldMaxY -> {worldEnv = pworldEnv, worldMaxX = pworldMaxX, worldMaxY = pworldMaxY})
   |> required "worldEnv" (jsonDecEnv)
   |> required "worldMaxX" (Json.Decode.int)
   |> required "worldMaxY" (Json.Decode.int)

jsonEncWorld : World -> Value
jsonEncWorld  val =
   Json.Encode.object
   [ ("worldEnv", jsonEncEnv val.worldEnv)
   , ("worldMaxX", Json.Encode.int val.worldMaxX)
   , ("worldMaxY", Json.Encode.int val.worldMaxY)
   ]



type alias Cell  =
   { cellEnv: WorldId
   , cellX: Int
   , cellY: Int
   , cellColor: Color
   , cellValue: String
   , cellCType: CellType
   }

jsonDecCell : Json.Decode.Decoder ( Cell )
jsonDecCell =
   Json.Decode.succeed (\pcellEnv pcellX pcellY pcellColor pcellValue pcellCType -> {cellEnv = pcellEnv, cellX = pcellX, cellY = pcellY, cellColor = pcellColor, cellValue = pcellValue, cellCType = pcellCType})
   |> required "cellEnv" (jsonDecWorldId)
   |> required "cellX" (Json.Decode.int)
   |> required "cellY" (Json.Decode.int)
   |> required "cellColor" (jsonDecColor)
   |> required "cellValue" (Json.Decode.string)
   |> required "cellCType" (jsonDecCellType)

jsonEncCell : Cell -> Value
jsonEncCell  val =
   Json.Encode.object
   [ ("cellEnv", jsonEncWorldId val.cellEnv)
   , ("cellX", Json.Encode.int val.cellX)
   , ("cellY", Json.Encode.int val.cellY)
   , ("cellColor", jsonEncColor val.cellColor)
   , ("cellValue", Json.Encode.string val.cellValue)
   , ("cellCType", jsonEncCellType val.cellCType)
   ]



type alias Message  =
   { messageEnv: WorldId
   , messageSenderId: UserId
   , messageValue: String
   }

jsonDecMessage : Json.Decode.Decoder ( Message )
jsonDecMessage =
   Json.Decode.succeed (\pmessageEnv pmessageSenderId pmessageValue -> {messageEnv = pmessageEnv, messageSenderId = pmessageSenderId, messageValue = pmessageValue})
   |> required "messageEnv" (jsonDecWorldId)
   |> required "messageSenderId" (jsonDecUserId)
   |> required "messageValue" (Json.Decode.string)

jsonEncMessage : Message -> Value
jsonEncMessage  val =
   Json.Encode.object
   [ ("messageEnv", jsonEncWorldId val.messageEnv)
   , ("messageSenderId", jsonEncUserId val.messageSenderId)
   , ("messageValue", Json.Encode.string val.messageValue)
   ]



type alias User  =
   { userEnv: WorldId
   , userName: String
   , userX: Int
   , userY: Int
   , userFacing: Direction
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\puserEnv puserName puserX puserY puserFacing -> {userEnv = puserEnv, userName = puserName, userX = puserX, userY = puserY, userFacing = puserFacing})
   |> required "userEnv" (jsonDecWorldId)
   |> required "userName" (Json.Decode.string)
   |> required "userX" (Json.Decode.int)
   |> required "userY" (Json.Decode.int)
   |> required "userFacing" (jsonDecDirection)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("userEnv", jsonEncWorldId val.userEnv)
   , ("userName", Json.Encode.string val.userName)
   , ("userX", Json.Encode.int val.userX)
   , ("userY", Json.Encode.int val.userY)
   , ("userFacing", jsonEncDirection val.userFacing)
   ]



type Env  =
    Dev 
    | Prod 

jsonDecEnv : Json.Decode.Decoder ( Env )
jsonDecEnv = 
    let jsonDecDictEnv = Dict.fromList [("Dev", Dev), ("Prod", Prod)]
    in  decodeSumUnaries "Env" jsonDecDictEnv

jsonEncEnv : Env -> Value
jsonEncEnv  val =
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



type Direction  =
    North 
    | South 
    | East 
    | West 

jsonDecDirection : Json.Decode.Decoder ( Direction )
jsonDecDirection = 
    let jsonDecDictDirection = Dict.fromList [("North", North), ("South", South), ("East", East), ("West", West)]
    in  decodeSumUnaries "Direction" jsonDecDictDirection

jsonEncDirection : Direction -> Value
jsonEncDirection  val =
    case val of
        North -> Json.Encode.string "North"
        South -> Json.Encode.string "South"
        East -> Json.Encode.string "East"
        West -> Json.Encode.string "West"


getWorldIdByEnv : Env -> (Result Http.Error  ((Maybe (WorldId)))  -> msg) -> Cmd msg
getWorldIdByEnv capture_env toMsg =
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
                    [ "worldId"
                    , (capture_env |> strEncEnv)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe jsonDecWorldId)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getWorldByWid : (WorldId) -> (Result Http.Error  ((Maybe World))  -> msg) -> Cmd msg
getWorldByWid capture_wid toMsg =
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
                    [ "world"
                    , (capture_wid |> String.fromInt)
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

getGridByWid : (WorldId) -> (Result Http.Error  ((List Cell))  -> msg) -> Cmd msg
getGridByWid capture_wid toMsg =
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
                    [ "grid"
                    , (capture_wid |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecCell))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getMsgsByWidByRecentN : (WorldId) -> Int -> (Result Http.Error  ((List Message))  -> msg) -> Cmd msg
getMsgsByWidByRecentN capture_wid capture_recentN toMsg =
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
                    [ "msgs"
                    , (capture_wid |> String.fromInt)
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

getUserByUid : (UserId) -> (Result Http.Error  ((Maybe User))  -> msg) -> Cmd msg
getUserByUid capture_uid toMsg =
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
                    [ "user"
                    , (capture_uid |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecUser))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUsersByWid : (WorldId) -> (Result Http.Error  ((List User))  -> msg) -> Cmd msg
getUsersByWid capture_wid toMsg =
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
                    [ "users"
                    , (capture_wid |> String.fromInt)
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

putMoveByUidByDirection : (UserId) -> Direction -> (Result Http.Error  (Direction)  -> msg) -> Cmd msg
putMoveByUidByDirection capture_uid capture_direction toMsg =
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
                    [ "move"
                    , (capture_uid |> String.fromInt)
                    , (capture_direction |> strEncDirection)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecDirection
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putRefaceByUidByDirection : (UserId) -> Direction -> (Result Http.Error  (Direction)  -> msg) -> Cmd msg
putRefaceByUidByDirection capture_uid capture_direction toMsg =
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
                    [ "reface"
                    , (capture_uid |> String.fromInt)
                    , (capture_direction |> strEncDirection)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecDirection
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putCellColorByWidByXByYByColor : (WorldId) -> Int -> Int -> Color -> (Result Http.Error  (Color)  -> msg) -> Cmd msg
putCellColorByWidByXByYByColor capture_wid capture_x capture_y capture_color toMsg =
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
                    [ "cellColor"
                    , (capture_wid |> String.fromInt)
                    , (capture_x |> String.fromInt)
                    , (capture_y |> String.fromInt)
                    , (capture_color |> strEncColor)
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

putClearCellByWidByXByY : (WorldId) -> Int -> Int -> (Result Http.Error  (Bool)  -> msg) -> Cmd msg
putClearCellByWidByXByY capture_wid capture_x capture_y toMsg =
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
                    [ "clearCell"
                    , (capture_wid |> String.fromInt)
                    , (capture_x |> String.fromInt)
                    , (capture_y |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.bool)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putCellValueByWidByXByYByVal : (WorldId) -> Int -> Int -> String -> (Result Http.Error  (String)  -> msg) -> Cmd msg
putCellValueByWidByXByYByVal capture_wid capture_x capture_y capture_val toMsg =
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
                    [ "cellValue"
                    , (capture_wid |> String.fromInt)
                    , (capture_x |> String.fromInt)
                    , (capture_y |> String.fromInt)
                    , (capture_val)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.string)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

-- Post Code Gen Appends (after servant-elm)

-- Add aliases to resolve erasure of (Key a) types from persistent

type alias WorldId = Int
type alias CellId = Int
type alias MessageId = Int
type alias UserId = Int

jsonEncWorldId = Json.Encode.int
jsonDecWorldId = Json.Decode.int
jsonEncCellId = Json.Encode.int
jsonDecCellId = Json.Decode.int
jsonEncMessageId = Json.Encode.int
jsonDecMessageId = Json.Decode.int
jsonEncUserId = Json.Encode.int
jsonDecUserId = Json.Decode.int

strEncEnv : Env -> String
strEncEnv val = 
    case val of 
        Dev -> "Dev"
        Prod -> "Prod"

strEncColor : Color -> String
strEncColor val = 
    case val of 
        White -> "White"
        Yellow -> "Yellow"
        Red -> "Red"
        Green -> "Green"
        Blue -> "Blue"
        Grey -> "Grey"

strEncCellType : CellType -> String
strEncCellType val = 
    case val of 
        Std -> "Std"
        Link -> "Link"
        Text -> "Text"
        Fixed -> "Fixed"

strEncDirection : Direction -> String
strEncDirection val = 
    case val of 
        North -> "North"
        South -> "South"
        East -> "East"
        West -> "West"

cycleColor : Color -> Color
cycleColor val = 
    case val of 
        White -> Yellow
        Yellow -> Red
        Red -> Green
        Green -> Blue
        Blue -> Grey
        Grey -> White

cycleCellType : CellType -> CellType
cycleCellType val = 
    case val of 
        Std -> Link
        Link -> Text
        Text -> Fixed
        Fixed -> Std

