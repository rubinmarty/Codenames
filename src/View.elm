module View exposing (view)

import Types exposing (..)
import Vector exposing (Vector)
import Grid exposing (render)
import State exposing (cardsRemaining)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
            (++) "Cards Remaining: " <| toString <| cardsRemaining model.board <| Team team
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