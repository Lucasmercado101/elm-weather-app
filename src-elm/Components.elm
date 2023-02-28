module Components exposing (..)

import Element exposing (Color, Element, centerX, column, el, paddingXY, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Localizations exposing (secondaryColor)
import Material.Icons.Types exposing (Coloring(..), Icon)
import Utils exposing (paddingBottom)


statCard : Color -> Icon msg -> String -> String -> Element msg
statCard color icon title value =
    column
        [ spacing 12
        , Font.color color
        ]
        [ el [ centerX, paddingBottom 8, Font.color color ] (icon 52 Inherit |> Element.html)
        , el [ Font.regular, Font.size 24, centerX ] (text value)
        , el [ centerX, Font.size 14, Font.light ] (text title)
        ]


toggle :
    { primaryColor : Color
    , secondaryColor : Color
    , isToggled : Bool
    , values : ( String, String )
    }
    -> Element msg
toggle { primaryColor, secondaryColor, isToggled, values } =
    let
        ( on, off ) =
            values
    in
    row
        [ Border.color primaryColor
        , Border.width 3
        ]
        [ el
            [ paddingXY 8 5
            , Font.heavy
            , if isToggled then
                Font.color primaryColor

              else
                Font.color secondaryColor
            , if isToggled then
                Background.color secondaryColor

              else
                Background.color primaryColor
            ]
            (text on)
        , el
            [ Font.heavy
            , paddingXY 8 5
            , if isToggled then
                Font.color secondaryColor

              else
                Font.color primaryColor
            , if isToggled then
                Background.color primaryColor

              else
                Background.color secondaryColor
            ]
            (text off)
        ]
