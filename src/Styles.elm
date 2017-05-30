module Styles
    exposing
        ( styles
        , buttonStyle
        , cardStyle
        , sashStyle
        , remainingBoxStyle
        , clueSectionStyle
        , clueDisplayStyle
        , clueDisplayEntryStyle
        )

import Css
    exposing
        ( Color
        , Mixin
        , asPairs
        , property
        , rgb
        , hex
        , fontSize
        , margin
        , pct
        , px
        , default
        , pointer
        , cursor
        , border3
        , solid
        , color
        , backgroundColor
        , height
        , lineHeight
        , width
        , display
        , inlineBlock
        , position
        , relative
        , borderRadius
        , textAlign
        , center
        , fontFamilies
        , sansSerif
        , fontSize
        , overflow
        , hidden
        , transform
        , rotate
        , deg
        , absolute
        , left
        , top
        , textIndent
        , displayFlex
        , dashed
        , overflowY
        , auto
        )
import Html
import Html.Attributes
import Types exposing (Card, CardType(Blank, KillWord, Team), Team(Blue, Red))


styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Html.Attributes.style



{- Custom Properties -}


type UserSelectValue
    = None


userSelect : UserSelectValue -> Mixin
userSelect value =
    case value of
        None ->
            property "user-select" "none"


type ContentValue
    = StringValue String


content : ContentValue -> Mixin
content value =
    case value of
        StringValue str ->
            property "content" str



{- Colors -}


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


toCssColor : ColorName -> Color
toCssColor name =
    case name of
        Black ->
            rgb 0 0 0

        White ->
            rgb 255 255 255

        Gray ->
            rgb 128 128 128

        Beige ->
            rgb 240 232 196

        RedColor ->
            rgb 255 0 0

        BlueColor ->
            rgb 0 0 255

        LightRed ->
            hex "E88"

        LightBlue ->
            hex "ACC"

        BrightBlue ->
            hex "5AF"


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



{- Styles -}


buttonStyle : List Mixin
buttonStyle =
    [ fontSize (pct 180)
    , margin (px 4)
    ]


cardStyle : Card -> Bool -> List Mixin
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

        cursorStyle =
            if isGameOver then
                default
            else
                pointer
    in
        [ cursor cursorStyle
        , border3 (px 5) solid borderColor
        , color fontColor
        , backgroundColor displayColor
        , height (px 90)
        , lineHeight (px 90)
        , width (px 180)
        , display inlineBlock
        , position relative
        , margin (px 1)
        , borderRadius (px 20)
        , textAlign center
        , fontFamilies [ "Helvetica", .value sansSerif ]
        , fontSize (px 26)
        , userSelect None
        , overflow hidden
        ]


sashStyle : CardType -> List Mixin
sashStyle cardType =
    [ content (StringValue "''")
    , width (px 10)
    , height (px 50)
    , transform (rotate (deg 45))
    , position absolute
    , left (px 8)
    , top (px -12)
    , backgroundColor (toCssColor <| cardColor cardType)
    ]


remainingBoxStyle : Team -> Bool -> List Mixin
remainingBoxStyle team active =
    let
        borderWidth =
            if active then
                px 3
            else
                px 1
    in
        [ width (px 478)
        , height (px 48)
        , display inlineBlock
        , fontSize (px 30)
        , textIndent (px 10)
        , lineHeight (px 45)
        , border3 borderWidth solid (toCssColor <| teamColor team)
        , backgroundColor (toCssColor <| teamBackgroundColor team)
        ]


clueSectionStyle : List Mixin
clueSectionStyle =
    [ displayFlex
    , margin (px 5)
    , border3 (px 1) dashed (toCssColor Black)
    ]


clueDisplayStyle : List Mixin
clueDisplayStyle =
    [ width (px 200)
    , margin (px 4)
    , overflowY auto
    ]


clueDisplayEntryStyle : Team -> List Mixin
clueDisplayEntryStyle team =
    [ color (toCssColor <| teamColor team) ]
