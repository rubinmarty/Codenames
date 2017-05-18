import Html exposing (Html, Attribute, div, button, text, span)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (andThen, withDefault)
import Grid exposing (Grid, render, lookupV, setV, grid)
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
  , isOver : Bool
  , wordList : WordList
  }
newModel : Model
newModel =
  { board = blankBoard
  , turn = Team1
  , hints = False
  , isOver = False
  , wordList = NormalWords
  }


type Team = Team1 | Team2
otherTeam : Team -> Team
otherTeam team =
  case team of
    Team1 -> Team2
    Team2 -> Team1

type Owner = Blank | KillWord | Team Team
ownerList : Team -> List Owner
ownerList activeTeam =
  List.repeat 9 (Team activeTeam)
  ++ List.repeat 8 (Team <| otherTeam activeTeam)
  ++ List.repeat 7 Blank
  ++ List.singleton KillWord

type alias Card =
  { word : String
  , owner : Owner
  , revealed : Bool
  , mouseOver : Bool
  }
dummyCard : Card
dummyCard = 
  { word = "test"
  , owner = Blank
  , revealed = False
  , mouseOver = False
  }

type alias Board = Grid Card
blankBoard : Board
blankBoard = grid 5 5 dummyCard

type WordList = SmallWords | NormalWords | OriginalWords
getWordList : WordList -> List String
getWordList wl =
  case wl of
    SmallWords -> WordLists.small_words
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
  | SetCardOwners (List Owner)
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
randomTeam = Random.generate SetTeam (Random.map (\b -> if b then Team1 else Team2) Random.bool)

randomCards : Team -> Cmd Msg
randomCards t =
  Random.generate SetCardOwners <| shuffle <| ownerList t

randomWords : WordList -> Cmd Msg
randomWords wl =
  Random.generate SetCardWords <| shuffle <| getWordList wl

setCardOwners : List Owner -> Board -> Board
setCardOwners list board =
  let index v = (getX v) + (5 * getY v) in
  board
  |> Grid.indexedMap (\v card -> {card | owner = withDefault Blank <| get (index v) list})

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
  {model | turn = if model.turn == Team1 then Team2 else Team1}

endGame : Model -> Model
endGame model =
  let
    remaining = cardsRemaining model.board
  in
    if List.any ((==) 0 << remaining) [Team Team1, Team Team2, KillWord]
    then {model | isOver = True}
    else model

click : Vector -> Model -> Model
click v model =
  if model.isOver then model else
  let card = lookupV v model.board |> withDefault dummyCard in
  if card.revealed then model else
  endGame <|
  reveal v <|
  case card.owner of
    Blank -> passTurn model
    KillWord -> passTurn model
    Team t -> if t /= model.turn
      then passTurn model
      else model



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

cardColor : Card -> String
cardColor card =
  if card.revealed
  then
    ownerColor card.owner
  else
    "rgb(240,232,196)" --Beige

ownerColor : Owner -> String
ownerColor o =
  case o of
    Blank -> "Gray"
    KillWord -> "Black"
    Team t -> teamColor t

teamColor : Team -> String
teamColor t =
  case t of
    Team1 -> "Blue"
    Team2 -> "Red"
  

tileStyle : Card -> Bool -> Attribute msg
tileStyle card isOver =
  let
    fontColor card =
      if (card.revealed && card.owner == KillWord) then [("color", "white")] else []
  in
    style <|
      [ ("height", "90px")
      , ("width", "180px")
      , ("display", "inline-block")
      , ("background-color", cardColor card)
      , ("position", "relative")
      , ("margin", "1px")
      , ("border", "5px solid " ++ borderColor card)
      , ("border-radius", "20px")
      , ("text-align", "center")
      , ("line-height", "270%")
      , ("font-size", "30px")
      , ("overflow", "hidden")
      ]
      ++ fontColor card
      ++ [("cursor", if isOver then "default" else "pointer")]


borderColor card =
  if card.mouseOver then "#5AF" else "Black"

tile : Vector -> Card -> Bool -> Bool -> Html Msg
tile v card hasHints isOver =
  span
    [onClick <| Click v, onMouseEnter <| EnterTile v, onMouseLeave <| LeaveTile v, tileStyle card isOver]
    ([text card.word] ++ if hasHints then [sash card.owner] else [])

sash : Owner -> Html Msg
sash owner =
  div [style
    [ ("content", "''")
    , ("width", "10px")
    , ("height", "50px")
    , ("transform", "rotate(45deg)")
    , ("position", "absolute")
    , ("left", "8px")
    , ("top", "-12px")
    , ("background-color", ownerColor owner)
    ]
  ] []

resetButton : Html Msg
resetButton = button [onClick Reset] [text "Reset"]

hintsButton : Bool -> Html Msg
hintsButton slns =
  button
    [onClick ToggleHints, style [("width","110px")]]
    [text <| if slns then "Hide Solutions" else "View Solutions"]

remainingBox : Model -> Team -> Html Msg
remainingBox model team =
  div
    [style
      [ ("width", "478px")
      , ("height", "48px")
      , ("border", if model.turn == team then "3px solid " ++ teamColor team else "1px solid black")
      , ("display", "inline-block")
      , ("font-size", "30px")
      , ("text-indent", "10px")
      , ("line-height", "45px")
      , ("background-color", teamBackgroundColor team )
      ]
    ]
    [text <| (++) "Cards remaining: " <| toString <| cardsRemaining model.board <| Team team ]

teamBackgroundColor : Team -> String
teamBackgroundColor team =
  case team of
    Team1 -> "#ACC" --lightblue
    Team2 -> "#E88" --lightred

cardsRemaining : Board -> Owner -> Int
cardsRemaining board owner =
  let
    doesCount card = card.owner == owner && card.revealed == False
  in
    Grid.allVectors board
    |> List.filter (\v -> lookupV v board |> Maybe.map doesCount |> withDefault False)
    |> List.length

wordListButton : WordList -> Html Msg
wordListButton wl =
  span
    []
    [ Html.input [type_ "radio", name "wordList", onClick <| SetWordList wl, checked <| wl == NormalWords] []
    , text <| toString wl
    ]

view : Model -> Html Msg
view model =
  div
    []
    <|
    [ resetButton, hintsButton model.hints
    , Html.br [] [], wordListButton SmallWords, wordListButton NormalWords, wordListButton OriginalWords
    , render (\a b -> tile a b model.hints model.isOver) model.board
    , remainingBox model Team1, remainingBox model Team2]