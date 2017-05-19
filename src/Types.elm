module Types exposing (..)

import Grid exposing (Grid)
import Vector exposing (Vector)



type alias Model =
    { board : Board
    , turn : Team
    , hints : Bool
    , isGameOver : Bool
    , wordList : WordList
    }


type Team = Blue | Red
otherTeam : Team -> Team
otherTeam team =
    case team of
        Blue -> Red
        Red -> Blue


type CardType = Blank | KillWord | Team Team


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


type alias Board = Grid Card


type WordList = EasyWords | NormalWords | OriginalWords


type Msg =
      Click Vector
    | InitState (Team, List CardType, List String)
    | SetWordList WordList
    | ToggleHints
    | Reset
    | MouseOverTile Bool Vector
    | ReceiveMessage Transmission


type alias Transmission =
    { wordList : Maybe (List String)
    , typeList : Maybe (List CardType)
    , click : Maybe Vector
    , turn : Maybe Team
    , isGameOver : Bool
    , reset : Bool
    }

blank : Transmission
blank =
    { wordList   = Nothing
    , typeList   = Nothing
    , click      = Nothing
    , turn       = Nothing
    , isGameOver = False
    , reset      = False
    }