module View exposing (view)

import Css
import Grid exposing (Grid)
import Html exposing (Html, text, span, div, button, input, label, br)
import Html.Attributes exposing (type_, name, checked, title, placeholder, value, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onInput, onSubmit)
import State exposing (cardsRemaining)
import Types exposing (Model, Msg(..), CardType(..), Team(..), WordList(..), Card)
import Vector exposing (Vector)


----------------------
--------COLORS--------
----------------------


type ColorName
    = Black
    | White
    | Gray
    | Beige
    | RedColor
    | BlueColor
    | LightBlue
    | LightRed
    | BrightBlue


toCssColor : ColorName -> Css.Color
toCssColor name =
    case name of
        Black ->
            Css.rgb 0 0 0

        White ->
            Css.rgb 255 255 255

        Gray ->
            Css.rgb 128 128 128

        Beige ->
            Css.rgb 240 232 196

        RedColor ->
            Css.rgb 255 0 0

        BlueColor ->
            Css.rgb 0 0 255

        LightRed ->
            Css.hex "E88"

        LightBlue ->
            Css.hex "ACC"

        BrightBlue ->
            Css.hex "5AF"


cardColor : CardType -> ColorName
cardColor o =
    case o of
        Blank ->
            Gray

        KillWord ->
            Black

        Team team ->
            teamColor team


teamColor : Team -> ColorName
teamColor t =
    case t of
        Blue ->
            BlueColor

        Red ->
            RedColor


teamBackgroundColor : Team -> ColorName
teamBackgroundColor team =
    case team of
        Blue ->
            LightBlue

        Red ->
            LightRed



----------------------
--------STYLES--------
----------------------


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


type UserSelectValue
    = None


userSelect : UserSelectValue -> Css.Mixin
userSelect value =
    case value of
        None ->
            Css.property "user-select" "none"


type ContentValue
    = StringValue String


content : ContentValue -> Css.Mixin
content value =
    case value of
        StringValue str ->
            Css.property "content" str


buttonStyle : List Css.Mixin
buttonStyle =
    [ Css.fontSize (Css.pct 180)
    , Css.margin (Css.px 4)
    ]


cardStyle : Card -> Bool -> List Css.Mixin
cardStyle card isGameOver =
    let
        fontColor =
            toCssColor <|
                if (card.revealed && card.cardType == KillWord) then
                    White
                else
                    Black

        borderColor =
            toCssColor <|
                if card.mouseOver then
                    BrightBlue
                else
                    Black

        displayColor =
            toCssColor <|
                if card.revealed then
                    cardColor card.cardType
                else
                    Beige

        cursor =
            if isGameOver then
                Css.default
            else
                Css.pointer
    in
        [ Css.cursor cursor
        , Css.border3 (Css.px 5) Css.solid borderColor
        , Css.color fontColor
        , Css.backgroundColor displayColor
        , Css.height (Css.px 90)
        , Css.lineHeight (Css.px 90)
        , Css.width (Css.px 180)
        , Css.display Css.inlineBlock
        , Css.position Css.relative
        , Css.margin (Css.px 1)
        , Css.borderRadius (Css.px 20)
        , Css.textAlign Css.center
        , Css.fontFamilies [ "Helvetica", .value Css.sansSerif ]
        , Css.fontSize (Css.px 26)
        , userSelect None
        , Css.overflow Css.hidden
        ]


sashStyle : CardType -> List Css.Mixin
sashStyle cardType =
    [ content (StringValue "''")
    , Css.width (Css.px 10)
    , Css.height (Css.px 50)
    , Css.transform (Css.rotate (Css.deg 45))
    , Css.position Css.absolute
    , Css.left (Css.px 8)
    , Css.top (Css.px -12)
    , Css.backgroundColor (toCssColor <| cardColor cardType)
    ]


remainingBoxStyle : Team -> Bool -> List Css.Mixin
remainingBoxStyle team active =
    let
        borderWidth =
            if active then
                Css.px 3
            else
                Css.px 1
    in
        [ Css.width (Css.px 478)
        , Css.height (Css.px 48)
        , Css.display Css.inlineBlock
        , Css.fontSize (Css.px 30)
        , Css.textIndent (Css.px 10)
        , Css.lineHeight (Css.px 45)
        , Css.border3 borderWidth Css.solid (toCssColor <| teamColor team)
        , Css.backgroundColor (toCssColor <| teamBackgroundColor team)
        ]


clueSectionStyle : List Css.Mixin
clueSectionStyle =
    [ Css.displayFlex
    , Css.margin (Css.px 5)
    , Css.border3 (Css.px 1) Css.dashed (toCssColor Black)
    ]


clueDisplayStyle : List Css.Mixin
clueDisplayStyle =
    [ Css.width (Css.px 200)
    , Css.margin (Css.px 4)
    , Css.overflowY Css.auto
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
            text <| (++) "Cards Remaining: " <| toString <| cardsRemaining model.board <| Team team
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
                [ div [ styles [ Css.color (toCssColor <| teamColor team) ] ]
                    [ text <| clue ++ " : " ++ toString num ]
                , div []
                    [ text <| (++) "> " <| String.join ", " <| List.reverse guesses ]
                ]
    in
        div [ styles clueDisplayStyle, id "chat" ] (List.reverse <| List.map entryRender model.log)


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
            span
                [ styles [ Css.padding (Css.px 5) ] ]
                [ resetButton, pass, hintsButton model.hints, br [] [], wordListButton EasyWords, br [] [], wordListButton OriginalWords, br [] [], wordListButton HardWords ]

        clueButtons =
            div [ styles clueSectionStyle ] [ clueInput model, clueDisplay model ]

        buttonArea =
            div [ styles [ Css.displayFlex, Css.height (Css.px 140) ] ] [ menuButtons, clueButtons ]

        cardArea =
            renderGrid (\a b -> cardNode a b model.hints model.isGameOver) model.board

        infoArea =
            div [] [ remainingBox model Blue, remainingBox model Red ]
    in
        div [] [ buttonArea, cardArea, infoArea ]
