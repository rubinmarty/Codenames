module Types exposing (..)

import Grid exposing (Grid)
import Vector exposing (Vector)
import Navigation



type alias Model =
    { board : Board
    , turn : Team
    , hints : Bool
    , isGameOver : Bool
    , wordList : WordList
    , serverAddress : String
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

    --triggered messages
      Send (List Msg)
    | Receive (List Msg)
    | Reset
    | InitState (Team, List CardType, List String)

    --sendable messages (should really be their own type)
    | SetClicked Vector
    | SetTurn Team
    | SetCardWords (List String)
    | SetCardTypes (List CardType)
    | PassTurn
    | NewGame

    --local messages
    | SetWordList WordList
    | SetHints Bool
    | MouseOverTile Bool Vector

    --unused messages
    | UrlChange Navigation.Location