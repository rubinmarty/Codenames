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
            , ("font-family", "calibri, helvetica, arial, sans-serif")
            , ("user-select", "none")
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
            [ onClick <| Send [SetClicked v]
            , onMouseEnter <| MouseOverTile True v
            , onMouseLeave <| MouseOverTile False v
            , cardStyle card isGameOver
            ]
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

passButton : Html Msg
passButton = button [onClick <| Send [PassTurn]] [text "Pass Turn"]

hintsButton : Bool -> Html Msg
hintsButton isHintsOn =
    let
        myStyle =
            style [("width","110px")]
        myText =
            if isHintsOn then "Hide Solutions" else "View Solutions"
        myClick =
            onClick <| SetHints <| not isHintsOn
    in
        button [myClick, myStyle] [text myText]

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
        myClick =
            onClick <| SetWordList wl
        isChecked =
            wl == NormalWords
        button_ =
            input [type_ "radio", name "wordList", myClick, checked isChecked] []
    in
        label [title title_] [button_, text text_]

clueInput : Model -> Html Msg
clueInput model =
    let
        b =
            br [] []
        clueBox =
            input [type_ "text", onInput SetClueBar, placeholder "Clue"] []
        numBox =
            input [type_ "number", onInput SetClueNumber, placeholder "0", value <| toString model.num] []
        newEntry =
            (model.turn, model.clue, model.num, [])
        myEvent =
            if model.clue /= "" && model.num /= 0
                then [onClick <| Send [LogPush newEntry]]
                else []
        clueButton =
            button myEvent [text "Submit"]
    in
        span [] [clueBox, b, numBox, b, clueButton]

clueDisplay : Model -> Html Msg
clueDisplay model =
    let
        render (team, clue, num, guesses) =
            div
                [style [("color", teamColor team)]]
                [ div [] [text <| clue ++ " : " ++ toString num]
                , div [] [text <| String.join ", " <| List.reverse guesses]
                ]
    in
        div [] (List.reverse <| List.map render model.log)

view : Model -> Html Msg
view model =
    let
        pad = 
            style [("padding","5px")]

        menuButtons =
            span [pad] [resetButton, passButton, hintsButton model.hints, br [] [], wordListButton EasyWords, br [] [], wordListButton NormalWords, br [] [], wordListButton OriginalWords]

        clueButtons =
            div [pad] [clueInput model, clueDisplay model]

        buttonArea =
            div [style [("display","flex")]] [menuButtons, clueButtons]

        cardArea =
            Grid.render (\a b -> cardNode a b model.hints model.isGameOver) model.board

        infoArea =
            div [] [remainingBox model Blue, remainingBox model Red]

    in
        div [] [buttonArea, cardArea, infoArea]