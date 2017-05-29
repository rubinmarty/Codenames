module View exposing (view)

import Css
import Grid exposing (Grid)
import Html exposing (Html, text, span, div, button, input, label, br)
import Html.Attributes exposing (type_, name, checked, title, placeholder, value, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onInput, onSubmit)
import State exposing (cardsRemaining)
import Styles exposing (styles, cardStyle, sashStyle, buttonStyle, remainingBoxStyle, clueDisplayStyle, clueDisplayEntryStyle, clueSectionStyle)
import Types exposing (Model, Msg(..), CardType(..), Team(..), WordList(..), Card)
import Vector exposing (Vector)


cardNode : Bool -> Bool -> Vector -> Card -> Html Msg
cardNode hasHints isGameOver v card  =
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
            styles <| cardStyle card isGameOver
    in
        span (myStyle :: myTriggers) [ myText, mySash ]


sash : CardType -> Html Msg
sash cardType =
    div [ styles <| sashStyle cardType ] []


resetButton : Html Msg
resetButton =
    button [ onClick Reset, styles buttonStyle ] [ text "New Game" ]


passButton : Bool -> Html Msg
passButton disabled =
    button
        [ onClick <| Send [ PassTurn ]
        , styles buttonStyle
        , Html.Attributes.disabled disabled
        ]
        [ text "Pass Turn" ]


hintsButton : Bool -> Html Msg
hintsButton isHintsOn =
    let
        myStyle =
            styles <| Css.minWidth (Css.px 240) :: buttonStyle

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
            Team team
                |> cardsRemaining model.board
                |> toString
                |> (++) "Cards Remaining: "
                |> text
    in
        div [ styles myStyle ] [ myText ]


wordListButton : WordList -> Html Msg
wordListButton wl =
    let
        ( text_, title_ ) =
            case wl of
                EasyWords ->
                    ( "Easy Words", "499 short words" )

                OriginalWords ->
                    ( "Original Words", "400 original words" )

                HardWords ->
                    ( "Hard Words", "" )

        myClick =
            onClick <| SetWordList wl

        isChecked =
            wl == OriginalWords

        button_ =
            input
                [ type_ "radio"
                , name "wordList"
                , myClick
                , checked isChecked
                ]
                []
    in
        label [ title title_ ] [ button_, text text_ ]


clueInput : Model -> Html Msg
clueInput model =
    let
        b =
            br [] []

        clueBox =
            input
                [ type_ "text"
                , onInput SetClueBar
                , placeholder "Clue"
                , value model.clue
                ]
                []

        numBox =
            input
                [ type_ "number"
                , onInput SetClueNumber
                , placeholder "0"
                , value <| toString model.num
                , Html.Attributes.min "1"
                , Html.Attributes.max "9"
                ]
                []

        clueButton =
            button [] [ text "Submit" ]

        clue =
            ( model.clue, model.num )

        myEvent =
            if model.clue /= "" && model.num /= 0 then
                onSubmit <| Send [ LogPush clue ]
            else
                onSubmit NoOp

        myStyle =
            styles [ Css.margin (Css.px 4) ]
    in
        Html.form [ myStyle, myEvent ] [ clueBox, b, numBox, b, clueButton ]


clueDisplay : Model -> Html Msg
clueDisplay model =
    let
        entryRender ( team, clue, num, guesses ) =
            div []
                [ div [ styles <| clueDisplayEntryStyle team ]
                    [ text <| clue ++ " : " ++ toString num ]
                , div []
                    [ List.reverse guesses
                        |> String.join ", "
                        |> (++) "> "
                        |> text
                    ]
                ]
    in
        div [ styles clueDisplayStyle, id "chat" ]
            (List.reverse <| List.map entryRender model.log)


renderGrid : (Vector -> a -> Html b) -> Grid a -> Html b
renderGrid f grid =
    Grid.indexedMap f grid
        |> Grid.foldRightBottom (::) (\x acc -> (div [] x) :: acc) [] []
        |> div [ styles [ Css.fontSize (Css.px 0) ] ]


view : Model -> Html Msg
view model =
    let
        pass =
            passButton <| not model.givenClue

        menuButtons =
            span [ styles [ Css.padding (Css.px 5) ] ]
                [ resetButton
                , pass
                , hintsButton model.hints
                , br [] []
                , wordListButton EasyWords
                , br [] []
                , wordListButton OriginalWords
                , br [] []
                , wordListButton HardWords
                ]

        clueButtons =
            div [ styles clueSectionStyle ]
                [ clueInput model, clueDisplay model ]

        buttonArea =
            div [ styles [ Css.displayFlex, Css.height (Css.px 140) ] ]
                [ menuButtons, clueButtons ]

        cardArea =
            renderGrid (cardNode model.hints model.isGameOver) model.board

        infoArea =
            div [] [ remainingBox model Blue, remainingBox model Red ]
    in
        div [] [ buttonArea, cardArea, infoArea ]
