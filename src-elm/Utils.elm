module Utils exposing (..)

import Element exposing (Color, Element)
import Html
import ParseInt
import Regex
import Time


paddingBottom : Int -> Element.Attribute msg
paddingBottom n =
    Element.paddingEach { top = 0, left = 0, right = 0, bottom = n }


paddingRight : Int -> Element.Attribute msg
paddingRight n =
    Element.paddingEach { top = 0, left = 0, right = n, bottom = 0 }


paddingLeft : Int -> Element.Attribute msg
paddingLeft n =
    Element.paddingEach { top = 0, left = n, right = 0, bottom = 0 }


paddingTop : Int -> Element.Attribute msg
paddingTop n =
    Element.paddingEach { top = n, left = 0, right = 0, bottom = 0 }


paddingX : Int -> Element.Attribute msg
paddingX n =
    Element.paddingEach { top = 0, left = n, right = n, bottom = 0 }


paddingY : Int -> Element.Attribute msg
paddingY n =
    Element.paddingEach { top = n, left = 0, right = 0, bottom = n }


br : Element msg
br =
    Element.html <| Html.br [] []


defaultPrimary : Color
defaultPrimary =
    Element.rgb255 254 225 66


defaultSecondary : Color
defaultSecondary =
    black


black : Color
black =
    Element.rgb 0 0 0


white : Color
white =
    Element.rgb 1 1 1


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


dayToString : Time.Weekday -> String
dayToString day =
    case day of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"



{- https://developer.mozilla.org/en-US/docs/Web/API/GeolocationPositionError#instance_properties -}


type GeoLocationApiError
    = PermissionDenied
    | PositionUnavailable
    | Timeout


codeToGeoLocationApiError : Int -> GeoLocationApiError
codeToGeoLocationApiError code =
    case code of
        1 ->
            PermissionDenied

        2 ->
            PositionUnavailable

        _ ->
            Timeout


geoLocationApiErrorToString : GeoLocationApiError -> String
geoLocationApiErrorToString error =
    case error of
        PermissionDenied ->
            "Permission to access location information was denied"

        PositionUnavailable ->
            "The location information is unavailable"

        Timeout ->
            "The request to get user location timed out"


type InitialWebRequest b
    = Loading
    | Failure b


type alias Coordinates =
    { latitude : Float, longitude : Float }



{-
   converted from https://github.com/eskimoblood/elm-color-extra/blob/5.1.0/src/Color/Convert.elm#L138
-}


hexToColor : String -> Result String Element.Color
hexToColor =
    let
        {- Converts "f" to "ff" and "ff" to "ff" -}
        extend : String -> String
        extend token =
            case String.toList token of
                [ t ] ->
                    String.fromList [ t, t ]

                _ ->
                    token

        pattern =
            ""
                ++ "^"
                ++ "#?"
                ++ "(?:"
                -- RRGGBB
                ++ "(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))"
                -- RGB
                ++ "|"
                ++ "(?:([a-f\\d])([a-f\\d])([a-f\\d]))"
                -- RRGGBBAA
                ++ "|"
                ++ "(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))"
                -- RGBA
                ++ "|"
                ++ "(?:([a-f\\d])([a-f\\d])([a-f\\d])([a-f\\d]))"
                ++ ")"
                ++ "$"
    in
    String.toLower
        >> Regex.findAtMost 1
            (Maybe.withDefault Regex.never <|
                Regex.fromString pattern
            )
        >> List.head
        >> Maybe.map .submatches
        >> Maybe.map (List.filterMap identity)
        >> Result.fromMaybe "Parsing hex regex failed"
        >> Result.andThen
            (\colors ->
                case List.map (extend >> ParseInt.parseIntHex) colors of
                    [ Ok r, Ok g, Ok b, Ok a ] ->
                        Ok <| Element.rgba255 r g b (roundToPlaces 2 (toFloat a / 255))

                    [ Ok r, Ok g, Ok b ] ->
                        Ok <| Element.rgba255 r g b 1

                    _ ->
                        -- there could be more descriptive error cases per channel
                        Err "Parsing ints from hex failed"
            )


roundToPlaces : Int -> Float -> Float
roundToPlaces places number =
    let
        multiplier =
            toFloat (10 ^ places)
    in
    toFloat (round (number * multiplier)) / multiplier



{-
   Converted from https://github.com/eskimoblood/elm-color-extra/blob/5.1.0/src/Color/Convert.elm#L252
-}


toHex : Int -> String
toHex i =
    ParseInt.toRadix 16 i |> Result.withDefault "" >> String.padLeft 2 '0'
