import Html exposing (Html, Attribute, div, button, text, span)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (andThen, withDefault)
import Grid exposing (Grid, lookupV, setV)
import Vector exposing (Vector, getX, getY)
import Random
import RandomList exposing (shuffle, get)
import WordLists

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , turn : Team
    , hints : Bool
    , isGameOver : Bool
    , wordList : WordList
    }
newModel : Model
newModel =
    { board = blankBoard
    , turn = Blue
    , hints = False
    , isGameOver = False
    , wordList = NormalWords
    }


type Team = Blue | Red
otherTeam : Team -> Team
otherTeam team =
    case team of
        Blue -> Red
        Red -> Blue

type CardType = Blank | KillWord | Team Team
cardTypeList : Team -> List CardType
cardTypeList activeTeam =
    List.repeat 9 (Team activeTeam)
    ++ List.repeat 8 (Team <| otherTeam activeTeam)
    ++ List.repeat 7 Blank
    ++ List.singleton KillWord

type alias Card =
    { word : String
    , cardType : CardType
    , revealed : Bool
    , mouseOver : Bool
    }
dummyCard : Card
dummyCard = 
    { word = "test"
    , cardType = Blank
    , revealed = False
    , mouseOver = False
    }

type alias Board = Grid Card
blankBoard : Board
blankBoard = Grid.grid 5 5 dummyCard

type WordList = EasyWords | NormalWords | OriginalWords
getWordList : WordList -> List String
getWordList wl =
    case wl of
        EasyWords -> WordLists.easy_words
        NormalWords -> WordLists.words
        OriginalWords -> WordLists.original



init : (Model, Cmd Msg)
init =
     reset newModel

reset : Model -> (Model, Cmd Msg)
reset model =
    {newModel | wordList = model.wordList} ! [randomTeam, randomWords model.wordList]



-- UPDATE


type Msg
    = Click Vector
    | SetTeam Team
    | SetCardOwners (List CardType)
    | SetCardWords (List String)
    | SetWordList WordList
    | ToggleHints
    | Reset
    | EnterTile Vector
    | LeaveTile Vector

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
            {model | board = setMouseOver2 True v model.board} ! []
        LeaveTile v ->
            {model | board = setMouseOver2 False v model.board} ! []

setMouseOver : Bool -> Card -> Card
setMouseOver b card =
    {card | mouseOver = b}

setMouseOver2 : Bool -> Vector -> Board -> Board
setMouseOver2 b v board =
    lookupV v board
    |> withDefault dummyCard
    |> setMouseOver b
    |> (\a b c d-> a b d c) setV v board 

randomTeam : Cmd Msg
randomTeam = Random.generate SetTeam (Random.map (\b -> if b then Blue else Red) Random.bool)

randomCards : Team -> Cmd Msg
randomCards t =
    Random.generate SetCardOwners <| shuffle <| cardTypeList t

randomWords : WordList -> Cmd Msg
randomWords wl =
    Random.generate SetCardWords <| shuffle <| getWordList wl

setCardOwners : List CardType -> Board -> Board
setCardOwners list board =
    let index v = (getX v) + (5 * getY v) in
    board
    |> Grid.indexedMap (\v card -> {card | cardType = withDefault Blank <| get (index v) list})

setCardWords : List String -> Board -> Board
setCardWords list board =
    let index v = (getX v) + (5 * getY v) in
    board
    |> Grid.indexedMap (\v card -> {card | word = withDefault "ERROR" <| get (index v) list})

reveal : Vector -> Model -> Model
reveal v model =
    lookupV v model.board
    |> andThen (\card -> Just 
        {model |
            board = setV v {card | revealed = True} model.board}
    )
    |> withDefault model

passTurn : Model -> Model
passTurn model =
    {model | turn = if model.turn == Blue then Red else Blue}

cardsRemaining : Board -> CardType -> Int
cardsRemaining board cardType =
    let
        doesCount card = card.cardType == cardType && card.revealed == False
    in
        Grid.allVectors board
        |> List.filter (\v -> lookupV v board |> Maybe.map doesCount |> withDefault False)
        |> List.length

endGame : Model -> Model
endGame model =
    let
        remaining = cardsRemaining model.board
    in
        if List.any ((==) 0 << remaining) [Team Blue, Team Red, KillWord]
        then {model | isGameOver = True}
        else model

click : Vector -> Model -> Model
click v model =
    if model.isGameOver then model else
    let card = lookupV v model.board |> withDefault dummyCard in
    if card.revealed then model else
    endGame <|
    reveal v <|
    case card.cardType of
        Blank -> passTurn model
        KillWord -> passTurn model
        Team t -> if t /= model.turn
            then passTurn model
            else model



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

cardColor : CardType -> String
cardColor o =
    case o of
        Blank -> "Gray"
        KillWord -> "Black"
        Team t -> teamColor t

teamColor : Team -> String
teamColor t =
    case t of
        Blue -> "Blue"
        Red -> "Red"

teamBackgroundColor : Team -> String
teamBackgroundColor team =
    case team of
        Blue -> "#ACC" --lightblue
        Red -> "#E88" --lightred
    

cardStyle : Card -> Bool -> Attribute msg
cardStyle card isGameOver =
    let
        fontColor =
            if (card.revealed && card.cardType == KillWord)
                then "white" 
                else "black"
        borderColor =
            if card.mouseOver
                then "#5AF"
                else "Black"
        displayColor =
            if card.revealed
                then cardColor card.cardType
                else "rgb(240,232,196)" --Beige
        cursor =
            if isGameOver
                then "default"
                else "pointer"
    in
        style
            [ ("cursor", cursor)
            , ("border", "5px solid " ++ borderColor)
            , ("color", fontColor)
            , ("background-color", displayColor )
            , ("height", "90px")
            , ("width", "180px")
            , ("display", "inline-block")
            , ("position", "relative")
            , ("margin", "1px")
            , ("border-radius", "20px")
            , ("text-align", "center")
            , ("line-height", "270%")
            , ("font-size", "30px")
            , ("overflow", "hidden")
            ]


cardNode : Vector -> Card -> Bool -> Bool -> Html Msg
cardNode v card hasHints isGameOver =
    let
        mySash =
            if hasHints
                then sash card.cardType
                else text ""
    in
        span
            [onClick <| Click v, onMouseEnter <| EnterTile v, onMouseLeave <| LeaveTile v, cardStyle card isGameOver]
            [text card.word, mySash]

sash : CardType -> Html Msg
sash cardType =
    let
        myStyle =
            style
                [ ("content", "''")
                , ("width", "10px")
                , ("height", "50px")
                , ("transform", "rotate(45deg)")
                , ("position", "absolute")
                , ("left", "8px")
                , ("top", "-12px")
                , ("background-color", cardColor cardType)
                ]
    in
        div [myStyle] []

resetButton : Html Msg
resetButton = button [onClick Reset] [text "Reset"]

hintsButton : Bool -> Html Msg
hintsButton isHintsOn =
    let
        myStyle =
            style [("width","110px")]
        myText =
            if isHintsOn then "Hide Solutions" else "View Solutions"
    in
        button [onClick ToggleHints, myStyle] [text myText]

remainingBox : Model -> Team -> Html Msg
remainingBox model team =
    let
        border =
            (if model.turn == team then "3" else "1") ++ "px solid " ++ teamColor team
        myStyle =
            style
                [ ("width", "478px")
                , ("height", "48px")
                , ("border", border)
                , ("display", "inline-block")
                , ("font-size", "30px")
                , ("text-indent", "10px")
                , ("line-height", "45px")
                , ("background-color", teamBackgroundColor team )
                ]
        str =
            (++) "Cards remaining: " <| toString <| cardsRemaining model.board <| Team team
    in
        div [myStyle] [text str]

wordListButton : WordList -> Html Msg
wordListButton wl =
    let
        (text_, title_) =
            case wl of
                EasyWords -> ("Easy Words", "499 short words")
                NormalWords -> ("Normal Words", "499 medium words")
                OriginalWords -> ("Original Words", "400 original words")

        button_ = Html.input [type_ "radio", title title_, name "wordList", onClick <| SetWordList wl, checked <| wl == NormalWords] []

    in
        div [] [button_, text text_]

view : Model -> Html Msg
view model =
    let
        menuButtons =
            div [] [resetButton, hintsButton model.hints]
        wordListButtons =
            div [] [wordListButton EasyWords, wordListButton NormalWords, wordListButton OriginalWords]
        buttonArea =
            div [] [menuButtons, wordListButtons]
        cardArea =
            Grid.render (\a b -> cardNode a b model.hints model.isGameOver) model.board
        infoArea =
            div [] [remainingBox model Blue, remainingBox model Red]
    in
        div [] [buttonArea, cardArea, infoArea]