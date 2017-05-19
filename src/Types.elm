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
    { word = "test"
    , cardType = Blank
    , revealed = False
    , mouseOver = False
    }



type alias Board = Grid Card



type WordList = EasyWords | NormalWords | OriginalWords




type Msg =
      Click Vector
    | SetTeam Team
    | SetCardOwners (List CardType)
    | SetCardWords (List String)
    | SetWordList WordList
    | ToggleHints
    | Reset
    | EnterTile Vector
    | LeaveTile Vector
    | NewMessage String