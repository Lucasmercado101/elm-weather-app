module Utils exposing (..)

import Element exposing (Color, Element)
import Html
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


primary : Color
primary =
    Element.rgb255 254 225 66


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


type Language
    = Spanish
    | English
