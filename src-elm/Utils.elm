module Utils exposing (..)

import Element exposing (Color, Element)
import Html
import Html.Attributes
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


type InitialWebRequest b
    = Loading
    | Failure b


type alias Coordinates =
    { latitude : Float, longitude : Float }


toIso8601BasicFormat : Time.Zone -> Time.Posix -> String
toIso8601BasicFormat zone time =
    let
        year : String
        year =
            Time.toYear zone time |> String.fromInt

        month : String
        month =
            Time.toMonth zone time |> monthToInt |> String.fromInt

        day : String
        day =
            Time.toDay zone time |> String.fromInt
    in
    year
        ++ "-"
        ++ (month
                |> (\l ->
                        if String.length l == 1 then
                            "0" ++ l

                        else
                            l
                   )
           )
        ++ "-"
        ++ (day
                |> (\l ->
                        if String.length l == 1 then
                            "0" ++ l

                        else
                            l
                   )
           )



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

        pattern : String
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
        multiplier : Float
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


type Location
    = UsingGeoLocation Coordinates
    | FixedCoordinates Coordinates


listMap6 :
    (a -> b -> c -> d -> e -> f -> result)
    -> List a
    -> List b
    -> List c
    -> List d
    -> List e
    -> List f
    -> List result
listMap6 fn a b c d e f =
    case ( a, b, c ) of
        ( x :: xs, y :: ys, z :: zs ) ->
            case ( d, e, f ) of
                ( xd :: ds, xe :: es, xf :: fs ) ->
                    fn x y z xd xe xf :: listMap6 fn xs ys zs ds es fs

                _ ->
                    []

        _ ->
            []


listMap7 :
    (a -> b -> c -> d -> e -> f -> g -> result)
    -> List a
    -> List b
    -> List c
    -> List d
    -> List e
    -> List f
    -> List g
    -> List result
listMap7 fn a b c d e f g =
    case ( a, b, c ) of
        ( x :: xs, y :: ys, z :: zs ) ->
            case ( d, e, f ) of
                ( xd :: ds, xe :: es, xf :: fs ) ->
                    case g of
                        xg :: gs ->
                            fn x y z xd xe xf xg :: listMap7 fn xs ys zs ds es fs gs

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


type alias PrimaryColor =
    Color


type alias SecondaryColor =
    Color


type alias Theme =
    ( PrimaryColor, SecondaryColor )


type alias RGB =
    ( Float, Float, Float )


noPointerEvents : Element.Attribute msg
noPointerEvents =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")


autoPointerEvents : Element.Attribute msg
autoPointerEvents =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")


nonEmptyString : String -> Maybe String
nonEmptyString v =
    if v == "" then
        Nothing

    else
        Just v


prepend : String -> String -> String
prepend value prefix =
    prefix ++ value
