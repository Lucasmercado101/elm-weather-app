module Components exposing (..)

import Element exposing (Color, Element, centerX, column, el, spacing, text)
import Element.Font as Font
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
