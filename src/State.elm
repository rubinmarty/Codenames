module State exposing (..)

import Dom.Scroll
import Grid
import List.Extra
import Maybe exposing (withDefault, andThen)
import Navigation
import Random exposing (Generator)
import Random.Extra
import Random.List
import Sockets
import Task
import Types exposing (Model, Msg(..), CardType(..), WordList(..), Team(..), Board, newModel, otherTeam)
import Vector exposing (Vector, getX, getY)
import WordLists


-- MODEL


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        socketProtocol =
            if location.protocol == "https:" then
                "wss://"
            else
                "ws://"
    in
        { newModel | serverAddress = socketProtocol ++ location.host } ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        send msgs =
            Sockets.send (model.serverAddress ++ "/submit") msgs
    in
        case msg of
            NoOp ->
                model ! []

            Send msgs ->
                model ! [ send msgs ]

            Receive msgs ->
                List.foldr (\x y -> update x <| Tuple.first y) (model ! []) msgs

            Reset ->
                model ! [ randomInitialState model.wordList ]

            InitState ( team, ctl, wl ) ->
                let
                    msgs =
                        [ NewGame
                        , SetTurn team
                        , SetCardTypes ctl
                        , SetCardWords wl
                        , SetHints False
                        ]
                in
                    model ! [ send msgs ]

            SetClicked v ->
                click v model

            SetCardTypes ctl ->
                setCardTypes ctl model ! []

            SetCardWords wl ->
                setCardWords wl model ! []

            SetTurn team ->
                setTurn team model ! []

            PassTurn ->
                setTurn (otherTeam model.turn) model ! []

            LogPush entry ->
                { model | log = entry :: model.log } ! [ scrollDown ]

            NewGame ->
                let
                    unrevealedBoard =
                        setUnrevealed model.board
                in
                    { model
                        | board = unrevealedBoard
                        , hints = False
                        , isGameOver = False
                        , log = []
                    }
                        ! []

            SetWordList wl ->
                { model | wordList = wl } ! []

            SetHints b ->
                { model | hints = b } ! []

            SetClueBar str ->
                let
                    str2 =
                        String.trim <| String.toUpper <| str
                in
                    { model | clue = str2 } ! []

            SetClueNumber str ->
                let
                    int =
                        String.toInt str
                            |> Result.withDefault 0
                            |> clamp 0 9
                in
                    { model | num = int } ! []

            MouseOverTile b v ->
                { model | board = setMouseOver b v model.board } ! []

            UrlChange location ->
                model ! []


click : Vector -> Model -> ( Model, Cmd Msg )
click v model =
    let
        isValidClick card =
            if model.isGameOver || card.revealed then
                Nothing
            else
                Just card
    in
        Grid.lookupV v model.board
            |> andThen isValidClick
            |> Maybe.map
                (\card ->
                    model
                        |> maybePassTurn card.cardType
                        |> reveal v
                        |> endGame
                        |> logGuess card.word
                )
            |> withDefault (model ! [])


scrollDown : Cmd Msg
scrollDown =
    Task.attempt (always NoOp) <| Dom.Scroll.toBottom "chat"


reveal : Vector -> Model -> Model
reveal v model =
    let
        setRevealed card =
            { card | revealed = True }
    in
        { model | board = Grid.update v setRevealed model.board }


maybePassTurn : CardType -> Model -> Model
maybePassTurn cardType model =
    case cardType of
        Blank ->
            passTurn model

        KillWord ->
            passTurn model

        Team t ->
            if t /= model.turn then
                passTurn model
            else
                model


passTurn : Model -> Model
passTurn model =
    { model | turn = otherTeam model.turn }


logGuess : String -> Model -> ( Model, Cmd Msg )
logGuess guess model =
    let
        addGuess ( a, b, c, words ) =
            ( a, b, c, guess :: words )

        updatedLog =
            List.Extra.updateAt 0 addGuess model.log
                |> Maybe.withDefault []
    in
        { model | log = updatedLog } ! [ scrollDown ]


setMouseOver : Bool -> Vector -> Board -> Board
setMouseOver isOver v board =
    let
        withMouseOver card =
            { card | mouseOver = isOver }
    in
        Grid.update v withMouseOver board


randomInitialState : WordList -> Cmd Msg
randomInitialState wordListType =
    let
        cardTypeList : Team -> List CardType
        cardTypeList activeTeam =
            List.repeat 9 (Team activeTeam)
                ++ List.repeat 8 (Team <| otherTeam activeTeam)
                ++ List.repeat 7 Blank
                ++ List.singleton KillWord

        wordList : List String
        wordList =
            case wordListType of
                EasyWords ->
                    WordLists.easy_words

                NormalWords ->
                    WordLists.words

                OriginalWords ->
                    WordLists.original

        randomTeam : Generator Team
        randomTeam =
            Random.Extra.choice Blue Red

        randomCards : Team -> Generator (List CardType)
        randomCards =
            Random.List.shuffle << cardTypeList

        randomWords : Generator (List String)
        randomWords =
            Random.map (List.take 25) <| Random.List.shuffle wordList
    in
        randomTeam
            |> Random.andThen
                (\team ->
                    Random.pair (Random.Extra.constant team) (randomCards team)
                )
            |> Random.map2
                (\word ( team, cardType ) ->
                    (,,) team cardType word
                )
                randomWords
            |> Random.generate InitState


setTurn : Team -> Model -> Model
setTurn team model =
    { model | turn = team }


setCardTypes : List CardType -> Model -> Model
setCardTypes cardTypes model =
    let
        getCardType v =
            List.Extra.getAt (getX v + 5 * getY v) cardTypes
                |> withDefault Blank

        setCardType v card =
            { card | cardType = getCardType v }
    in
        { model | board = Grid.indexedMap setCardType model.board }


setCardWords : List String -> Model -> Model
setCardWords cardWords model =
    let
        getWord v =
            List.Extra.getAt (getX v + 5 * getY v) cardWords
                |> withDefault "ERROR"

        setWord v card =
            { card | word = getWord v }
    in
        { model | board = Grid.indexedMap setWord model.board }


setUnrevealed : Board -> Board
setUnrevealed board =
    Grid.map (\card -> { card | revealed = False }) board


cardsRemaining : Board -> CardType -> Int
cardsRemaining board cardType =
    let
        doesCount card =
            card.cardType == cardType && card.revealed == False
    in
        Grid.countIf doesCount board


endGame : Model -> Model
endGame model =
    let
        noneRemaining : CardType -> Bool
        noneRemaining =
            ((==) 0) << cardsRemaining model.board
    in
        if List.any noneRemaining [ Team Blue, Team Red, KillWord ] then
            { model | isGameOver = True }
        else
            model


setGameOver : Bool -> Model -> Model
setGameOver b model =
    { model | isGameOver = b }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sockets.subscriptions (model.serverAddress ++ "/receive")
