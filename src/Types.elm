module Types exposing (..)

import Grid exposing (Grid)
import Navigation
import Vector exposing (Vector)


type alias Model =
    { board : Board
    , turn : Team
    , hints : Bool
    , isGameOver : Bool
    , wordList : WordList
    , log : Log
    , clue : String
    , num : Int
    , serverAddress : String
    }


newModel : Model
newModel =
    { board = blankBoard
    , turn = Blue
    , hints = False
    , isGameOver = True
    , wordList = NormalWords
    , log = []
    , clue = ""
    , num = 0
    , serverAddress = ""
    }


type Msg
    = NoOp
      --triggered messages
    | Send (List Msg)
    | Receive (List Msg)
    | Reset
    | InitState ( Team, List CardType, List String )
      --sendable messages (should really be their own type)
    | SetClicked Vector
    | SetTurn Team
    | SetCardWords (List String)
    | SetCardTypes (List CardType)
    | PassTurn
    | LogPush LogEntry
    | NewGame
      --local messages
    | SetWordList WordList
    | SetHints Bool
    | SetClueBar String
    | SetClueNumber String
    | MouseOverTile Bool Vector
      --unused messages
    | UrlChange Navigation.Location


type alias Board =
    Grid Card


blankBoard : Board
blankBoard =
    Grid.grid 5 5 dummyCard


type alias Card =
    { word : String
    , cardType : CardType
    , revealed : Bool
    , mouseOver : Bool
    }


dummyCard : Card
dummyCard =
    { word = ""
    , cardType = Blank
    , revealed = False
    , mouseOver = False
    }


type Team
    = Blue
    | Red


otherTeam : Team -> Team
otherTeam team =
    case team of
        Blue ->
            Red

        Red ->
            Blue


type CardType
    = Blank
    | KillWord
    | Team Team


type WordList
    = EasyWords
    | NormalWords
    | OriginalWords


type alias LogEntry =
    ( Team, String, Int, List String )


type alias Log =
    List LogEntry
