module View exposing (view)

import Grid exposing (Grid)
import Html exposing (Html, text, span, div, button, input, label, br)
import Html.Attributes exposing (style, type_, name, checked, title, placeholder, value, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onInput)
import State exposing (cardsRemaining)
import Types exposing (Model, Msg(..), CardType(..), Team(..), WordList(..), Card)
import Vector exposing (Vector)


----------------------
--------COLORS--------
----------------------


cardColor : CardType -> String
cardColor o =
    case o of
        Blank ->
            "Gray"

        KillWord ->
            "Black"

        Team t ->
            teamColor t


teamColor : Team -> String
teamColor t =
    case t of
        Blue ->
            "Blue"

        Red ->
            "Red"


teamBackgroundColor : Team -> String
teamBackgroundColor team =
    case team of
        Blue ->
            -- Light Blue
            "#ACC"

        Red ->
            -- Light Red
            "#E88"



----------------------
--------STYLES--------
----------------------


type alias Style =
    List ( String, String )


buttonStyle : Style
buttonStyle =
    [ ( "font-size", "180%" )
    , ( "margin", "4px" )
    ]


cardStyle : Card -> Bool -> Style
cardStyle card isGameOver =
    let
        fontColor =
            if (card.revealed && card.cardType == KillWord) then
                "white"
            else
                "black"

        borderColor =
            if card.mouseOver then
                "#5AF"
            else
                "Black"

        displayColor =
            if card.revealed then
                cardColor card.cardType
            else
                "rgb(240,232,196)"

        --Beige
        cursor =
            if isGameOver then
                "default"
            else
                "pointer"
    in
        [ ( "cursor", cursor )
        , ( "border", "5px solid " ++ borderColor )
        , ( "color", fontColor )
        , ( "background-color", displayColor )
        , ( "height", "90px" )
        , ( "line-height", "90px" )
        , ( "width", "180px" )
        , ( "display", "inline-block" )
        , ( "position", "relative" )
        , ( "margin", "1px" )
        , ( "border-radius", "20px" )
        , ( "text-align", "center" )
        , ( "font-family", "helvetica, sans-serif" )
        , ( "font-size", "26px" )
        , ( "user-select", "none" )
        , ( "overflow", "hidden" )
        ]


sashStyle : CardType -> Style
sashStyle cardType =
    [ ( "content", "''" )
    , ( "width", "10px" )
    , ( "height", "50px" )
    , ( "transform", "rotate(45deg)" )
    , ( "position", "absolute" )
    , ( "left", "8px" )
    , ( "top", "-12px" )
    , ( "background-color", cardColor cardType )
    ]


remainingBoxStyle : Team -> Bool -> Style
remainingBoxStyle team active =
    let
        borderWidth =
            if active then
                "3px"
            else
                "1px"
    in
        [ ( "width", "478px" )
        , ( "height", "48px" )
        , ( "display", "inline-block" )
        , ( "font-size", "30px" )
        , ( "text-indent", "10px" )
        , ( "line-height", "45px" )
        , ( "border-style", "solid" )
        , ( "border-width", borderWidth )
        , ( "border-color", teamColor team )
        , ( "background-color", teamBackgroundColor team )
        ]


clueSectionStyle : Style
clueSectionStyle =
    [ ( "display", "flex" )
    , ( "margin", "5px" )
    , ( "border", "1px dashed black" )
    ]


clueDisplayStyle : Style
clueDisplayStyle =
    [ ( "width", "200px" )
    , ( "margin", "4px" )
    , ( "overflow-y", "auto" )
    ]



----------------------
--------NODES---------
----------------------


cardNode : Vector -> Card -> Bool -> Bool -> Html Msg
cardNode v card hasHints isGameOver =
    let
        myText =
            text card.word

        mySash =
            if hasHints then
                sash card.cardType
            else
                text ""

        myTriggers =
            [ onClick <| Send [ SetClicked v ]
            , onMouseEnter <| MouseOverTile True v
            , onMouseLeave <| MouseOverTile False v
            ]

        myStyle =
            style <| cardStyle card isGameOver
    in
        span (myStyle :: myTriggers) [ myText, mySash ]


sash : CardType -> Html Msg
sash cardType =
    div [ style <| sashStyle cardType ] []


resetButton : Html Msg
resetButton =
    button [ onClick Reset, style buttonStyle ] [ text "New Game" ]


passButton : Html Msg
passButton =
    button [ onClick <| Send [ PassTurn ], style buttonStyle ] [ text "Pass Turn" ]


hintsButton : Bool -> Html Msg
hintsButton isHintsOn =
    let
        myStyle =
            style <| [ ( "min-width", "240px" ) ] ++ buttonStyle

        myText =
            if isHintsOn then
                "Hide Solutions"
            else
                "Show Solutions"

        myClick =
            onClick <| SetHints <| not isHintsOn
    in
        button [ myClick, myStyle ] [ text myText ]


remainingBox : Model -> Team -> Html Msg
remainingBox model team =
    let
        myStyle =
            remainingBoxStyle team (team == model.turn)

        myText =
            text <| (++) "Cards Remaining: " <| toString <| cardsRemaining model.board <| Team team
    in
        div [ style myStyle ] [ myText ]


wordListButton : WordList -> Html Msg
wordListButton wl =
    let
        ( text_, title_ ) =
            case wl of
                EasyWords ->
                    ( "Easy Words", "499 short words" )

                NormalWords ->
                    ( "Normal Words", "499 medium words" )

                OriginalWords ->
                    ( "Original Words", "400 original words" )

        myClick =
            onClick <| SetWordList wl

        isChecked =
            wl == NormalWords

        button_ =
            input [ type_ "radio", name "wordList", myClick, checked isChecked ] []
    in
        label [ title title_ ] [ button_, text text_ ]


clueInput : Model -> Html Msg
clueInput model =
    let
        b =
            br [] []

        clueBox =
            input [ type_ "text", onInput SetClueBar, placeholder "Clue", value model.clue ] []

        numBox =
            input [ type_ "number", onInput SetClueNumber, placeholder "0", value <| toString model.num ] []

        newEntry =
            ( model.turn, model.clue, model.num, [] )

        myEvent =
            if model.clue /= "" && model.num /= 0 then
                [ onClick <| Send [ LogPush newEntry ] ]
            else
                []

        clueButton =
            button myEvent [ text "Submit" ]
    in
        span [ style [ ( "margin", "4px" ) ] ] [ clueBox, b, numBox, b, clueButton ]


clueDisplay : Model -> Html Msg
clueDisplay model =
    let
        entryRender ( team, clue, num, guesses ) =
            div []
                [ div [ style [ ( "color", teamColor team ) ] ]
                    [ text <| clue ++ " : " ++ toString num ]
                , div []
                    [ text <| (++) "> " <| String.join ", " <| List.reverse guesses ]
                ]
    in
        div [ style clueDisplayStyle, id "chat" ] (List.reverse <| List.map entryRender model.log)


renderGrid : (Vector -> a -> Html b) -> Grid a -> Html b
renderGrid f grid =
    Grid.indexedMap f grid
        |> Grid.foldRightBottom (::) (\x acc -> (div [] x) :: acc) [] []
        |> div [ style [ ( "font-size", "0px" ) ] ]


view : Model -> Html Msg
view model =
    let
        menuButtons =
            span
                [ style [ ( "padding", "5px" ) ] ]
                [ resetButton, passButton, hintsButton model.hints, br [] [], wordListButton EasyWords, br [] [], wordListButton NormalWords, br [] [], wordListButton OriginalWords ]

        clueButtons =
            div [ style clueSectionStyle ] [ clueInput model, clueDisplay model ]

        buttonArea =
            div [ style [ ( "display", "flex" ), ( "height", "140px" ) ] ] [ menuButtons, clueButtons ]

        cardArea =
            renderGrid (\a b -> cardNode a b model.hints model.isGameOver) model.board

        infoArea =
            div [] [ remainingBox model Blue, remainingBox model Red ]
    in
        div [] [ buttonArea, cardArea, infoArea ]
