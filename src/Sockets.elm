module Sockets exposing (..)

import Types exposing (..)
import Vector exposing (Vector, getX, getY)

import WebSocket
import Result
import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)


subscriptions : String -> Sub Msg
subscriptions address =
    WebSocket.listen address (\s -> ReceiveMessage <| Maybe.withDefault blank <| deserialize s)

send : String -> Transmission -> Cmd Msg
send address state =
    WebSocket.send address <| serialize state



-- SERIALIZATION

serialize : Transmission -> String
serialize = Debug.log "serialize" << JE.encode 0 << transmissionE

deserialize : String -> Maybe Transmission
deserialize str =
    JD.decodeString transmissionD str
    |> Result.toMaybe
    |> Debug.log "deserialize"

-- ENCODING

vectorE : Vector -> JE.Value
vectorE v =
    JE.object [("x", JE.int <| getX v), ("y", JE.int <| getY v)]

maybeE : (a -> JE.Value) -> Maybe a -> JE.Value
maybeE f m =
    Maybe.withDefault JE.null <| Maybe.map f <| m

cardTypeE : CardType -> JE.Value
cardTypeE ct =
    JE.string <| toString ct

teamE : Team -> JE.Value
teamE = JE.string << toString

transmissionE : Transmission -> JE.Value
transmissionE tr =
    JE.object
        [ ("wordList", maybeE (JE.list << List.map JE.string) tr.wordList)
        , ("typeList", maybeE (JE.list << List.map cardTypeE) tr.typeList)
        , ("click", maybeE vectorE tr.click)
        , ("turn", maybeE teamE tr.turn)
        , ("isGameOver", JE.bool tr.isGameOver)
        , ("reset", JE.bool tr.reset)
        ]

-- DECODING

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    JD.oneOf
    [ JD.null Nothing
    , JD.map Just decoder
    ]

transmissionD : Decoder Transmission
transmissionD =
    JD.map6 Transmission
        (JD.field "wordList"   <| nullOr <| JD.list <| JD.string)
        (JD.field "typeList"   <| nullOr <| JD.list <| cardTypeD)
        (JD.field "click"      <| nullOr <| vectorD)
        (JD.field "turn"       <| nullOr <| teamD)
        (JD.field "isGameOver" <| JD.bool)
        (JD.field "reset"       <| JD.bool)

cardTypeD : Decoder CardType
cardTypeD =
    let
        helper str =
            case str of
                "KillWord"  -> JD.succeed KillWord
                "Blank"     -> JD.succeed Blank
                "Team Red"  -> JD.succeed (Team Red)
                "Team Blue" -> JD.succeed (Team Blue)
                _           -> JD.fail "Invalid CardType"
    in
        JD.andThen helper JD.string

teamD : Decoder Team
teamD =
    let
        helper str =
            case str of
                "Red"  -> JD.succeed Red
                "Blue" -> JD.succeed Blue
                _      -> JD.fail "Invalid Team"
    in
        JD.andThen helper JD.string

vectorD : Decoder Vector
vectorD = JD.map2 (,) (JD.field "x" JD.int) (JD.field "y" JD.int)
