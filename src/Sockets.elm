module Sockets exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Platform.Sub as Sub
import Result
import Types exposing (..)
import Vector exposing (Vector, getX, getY)
import WebSocket


subscriptions : String -> Sub Msg
subscriptions address =
    WebSocket.listen address (Receive << deserialize)


send : String -> List Msg -> Cmd Msg
send address msgs =
    WebSocket.send address <| serialize msgs



-- SERIALIZATION


debug : Bool
debug =
    False


print : String -> a -> a
print str =
    if debug then
        Debug.log str
    else
        identity


serialize : List Msg -> String
serialize =
    print "sending" << JE.encode 0 << msgsE << print "about to encode"


deserialize : String -> List Msg
deserialize =
    print "decoded" << Result.withDefault [] << JD.decodeString msgsD << print "received"



-- ENCODING


msgE : Msg -> Maybe JE.Value
msgE msg =
    case msg of
        SetClicked v ->
            Just <| JE.object [ ( "SetClicked", vectorE v ) ]

        SetTurn t ->
            Just <| JE.object [ ( "SetTurn", enumE t ) ]

        SetCardWords ls ->
            Just <| JE.object [ ( "SetCardWords", JE.list <| List.map JE.string ls ) ]

        SetCardTypes lct ->
            Just <| JE.object [ ( "SetCardTypes", JE.list <| List.map enumE lct ) ]

        PassTurn ->
            Just <| JE.object [ ( "PassTurn", JE.null ) ]

        LogPush entry ->
            Just <| JE.object [ ( "LogPush", logEntryE entry ) ]

        NewGame ->
            Just <| JE.object [ ( "NewGame", JE.null ) ]

        _ ->
            Nothing


msgsE : List Msg -> JE.Value
msgsE msgs =
    JE.list <| List.filterMap msgE msgs


enumE : a -> JE.Value
enumE =
    JE.string << toString


vectorE : Vector -> JE.Value
vectorE v =
    JE.object [ ( "x", JE.int <| getX v ), ( "y", JE.int <| getY v ) ]


logEntryE : LogEntry -> JE.Value
logEntryE ( team, clue, num, guesses ) =
    JE.object
        [ ( "team", enumE team )
        , ( "clue", JE.string clue )
        , ( "num", JE.int num )
        , ( "guesses", JE.list <| List.map JE.string guesses )
        ]



{-
   maybeE : (a -> JE.Value) -> Maybe a -> JE.Value
   maybeE f m =
       Maybe.withDefault JE.null <| Maybe.map f <| m
-}
-- DECODING


msgD : Decoder Msg
msgD =
    JD.oneOf
        [ JD.field "SetClicked" (JD.map SetClicked vectorD)
        , JD.field "SetTurn" (JD.map SetTurn teamD)
        , JD.field "SetCardWords" (JD.map SetCardWords (JD.list JD.string))
        , JD.field "SetCardTypes" (JD.map SetCardTypes (JD.list cardTypeD))
        , JD.field "PassTurn" (JD.succeed PassTurn)
        , JD.field "LogPush" (JD.map LogPush logEntryD)
        , JD.field "NewGame" (JD.succeed NewGame)
        ]


msgsD : Decoder (List Msg)
msgsD =
    JD.list msgD


logEntryD : Decoder LogEntry
logEntryD =
    JD.map4 (,,,)
        (JD.field "team" teamD)
        (JD.field "clue" JD.string)
        (JD.field "num" JD.int)
        (JD.field "guesses" <| JD.list JD.string)


cardTypeD : Decoder CardType
cardTypeD =
    let
        helper str =
            case str of
                "KillWord" ->
                    JD.succeed KillWord

                "Blank" ->
                    JD.succeed Blank

                "Team Red" ->
                    JD.succeed (Team Red)

                "Team Blue" ->
                    JD.succeed (Team Blue)

                _ ->
                    JD.fail "Invalid CardType"
    in
        JD.andThen helper JD.string


teamD : Decoder Team
teamD =
    let
        helper str =
            case str of
                "Red" ->
                    JD.succeed Red

                "Blue" ->
                    JD.succeed Blue

                _ ->
                    JD.fail "Invalid Team"
    in
        JD.andThen helper JD.string


vectorD : Decoder Vector
vectorD =
    JD.map2 (,) (JD.field "x" JD.int) (JD.field "y" JD.int)
