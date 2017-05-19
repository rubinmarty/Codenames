module State exposing (..)

import Types exposing (..)
import Vector exposing (..)
import Grid exposing (..)
import RandomList exposing (..)
import WordLists exposing (..)

import Random
import Maybe exposing (withDefault, andThen)
import WebSocket

-- MODEL

blankBoard : Board
blankBoard = Grid.grid 5 5 dummyCard

newModel : Model
newModel =
    { board = blankBoard
    , turn = Blue
    , hints = False
    , isGameOver = False
    , wordList = NormalWords
    }

init : (Model, Cmd Msg)
init =
     reset newModel

reset : Model -> (Model, Cmd Msg)
reset model =
    {newModel | wordList = model.wordList} ! [randomTeam, randomWords model.wordList]



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Click v ->
            click v model ! []
        SetTeam t ->
            {model | turn = t} ! [randomCards t]
        SetCardOwners list ->
            {model | board = setCardOwners list model.board} ! []
        SetCardWords list ->
            {model | board = setCardWords list model.board} ! []
        SetWordList wl ->
            {model | wordList = wl} ! []
        ToggleHints ->
            {model | hints = not model.hints} ! []
        Reset ->
            reset model
        EnterTile v ->
            {model | board = setMouseOver True v model.board} ! []
        LeaveTile v ->
            {model | board = setMouseOver False v model.board} ! []
        NewMessage str ->
            model ! []

withMouseOver : Bool -> Card -> Card
withMouseOver b card =
    {card | mouseOver = b}

setMouseOver : Bool -> Vector -> Board -> Board
setMouseOver b v board =
    lookupV v board
    |> Maybe.map (\card -> withMouseOver b card)
    |> Maybe.map (\card -> setV v card board)
    |> withDefault board

cardTypeList : Team -> List CardType
cardTypeList activeTeam =
    List.repeat 9 (Team activeTeam)
    ++ List.repeat 8 (Team <| otherTeam activeTeam)
    ++ List.repeat 7 Blank
    ++ List.singleton KillWord

getWordList : WordList -> List String
getWordList wl =
    case wl of
        EasyWords -> WordLists.easy_words
        NormalWords -> WordLists.words
        OriginalWords -> WordLists.original

randomTeam : Cmd Msg
randomTeam =
    Random.bool
    |> Random.map (\b -> if b then Blue else Red)
    |> Random.generate SetTeam

randomCards : Team -> Cmd Msg
randomCards t =
    Random.generate SetCardOwners <| shuffle <| cardTypeList t

randomWords : WordList -> Cmd Msg
randomWords wl =
    Random.generate SetCardWords <| shuffle <| getWordList wl

setCardOwners : List CardType -> Board -> Board
setCardOwners list board =
    let
        index v =
            (getX v) + (5 * getY v)
        setOwner v card =
            {card | cardType = withDefault Blank <| flip get list <| index v}
    in
        Grid.indexedMap setOwner board

setCardWords : List String -> Board -> Board
setCardWords list board =
    let
        index v =
            (getX v) + (5 * getY v)
        setWord v card =
            {card | word = withDefault "ERROR" <| flip get list <| index v}
    in
        Grid.indexedMap setWord board

reveal : Vector -> Model -> Model
reveal v model =
    let
        setRevealed card = 
            {card | revealed = True}
    in
        {model | board = Grid.mapAtV setRevealed v model.board}


passTurn : Model -> Model
passTurn model =
    {model | turn = otherTeam model.turn}

cardsRemaining : Board -> CardType -> Int
cardsRemaining board cardType =
    let
        doesCount card =
            card.cardType == cardType && card.revealed == False
        doesCount_ v =
            lookupV v board
            |> Maybe.map doesCount
            |> withDefault False
    in
        Grid.allVectors board
        |> List.filter doesCount_
        |> List.length

endGame : Model -> Model
endGame model =
    let
        noneRemaining : CardType -> Bool
        noneRemaining = ((==) 0) << cardsRemaining model.board
    in
        if List.any noneRemaining [Team Blue, Team Red, KillWord]
            then {model | isGameOver = True}
            else model

click : Vector -> Model -> Model
click v model =
    lookupV v model.board
    |> andThen (\card -> if model.isGameOver then Nothing else Just card)
    |> andThen (\card -> if card.revealed then Nothing else Just card)
    |> Maybe.map (\card -> case card.cardType of
                                Blank -> passTurn model
                                KillWord -> passTurn model
                                Team t -> if t /= model.turn
                                    then passTurn model
                                    else model)
    |> Maybe.map (reveal v)
    |> Maybe.map (endGame)
    |> withDefault model


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" NewMessage