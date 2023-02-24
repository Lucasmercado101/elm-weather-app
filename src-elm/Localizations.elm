module Localizations exposing (..)

import Element exposing (Element, none, text)


type Language
    = Spanish
    | English


dailySummary : Language -> String
dailySummary language =
    case language of
        English ->
            "Daily Summary"

        Spanish ->
            "Resumen Diario"


nowItFeels : Language -> Maybe Float -> Maybe Float -> Element msg
nowItFeels language apparentTemp actualTemp =
    case ( apparentTemp, actualTemp ) of
        ( Just apparent, Just actual ) ->
            case language of
                English ->
                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°, it's actually " ++ (actual |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora parece que hace " ++ (apparent |> String.fromFloat) ++ "°, pero en realidad hace " ++ (actual |> String.fromFloat) ++ "°")

        ( Just apparent, Nothing ) ->
            case language of
                English ->
                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora se siente como" ++ (apparent |> String.fromFloat) ++ "°")

        ( Nothing, Just actual ) ->
            case language of
                English ->
                    text ("Now it's " ++ (actual |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora hace " ++ (actual |> String.fromFloat) ++ "°")

        ( Nothing, Nothing ) ->
            -- NOTE: in theory should never happen
            none


temperatureRange : Language -> Maybe Float -> Maybe Float -> Element msg
temperatureRange language lowestTemp highestTemp =
    case ( lowestTemp, highestTemp ) of
        ( Just lowest, Just highest ) ->
            case language of
                English ->
                    text ("Today, the temperature is felt in the range from " ++ (lowest |> String.fromFloat) ++ "° to " ++ (highest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura se siente en el rango de " ++ (lowest |> String.fromFloat) ++ "° a " ++ (highest |> String.fromFloat) ++ "°")

        ( Just lowest, Nothing ) ->
            case language of
                English ->
                    text ("Today, the temperature lowest temperature is " ++ (lowest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura más baja es " ++ (lowest |> String.fromFloat) ++ "°")

        ( Nothing, Just highest ) ->
            case language of
                English ->
                    text ("Today, the temperature highest temperature is " ++ (highest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura más alta es " ++ (highest |> String.fromFloat) ++ "°")

        ( Nothing, Nothing ) ->
            none


wind : Language -> String
wind language =
    case language of
        English ->
            "Wind"

        Spanish ->
            "Viento"


humidity : Language -> String
humidity language =
    case language of
        English ->
            "Humidity"

        Spanish ->
            "Humedad"


visibility : Language -> String
visibility language =
    case language of
        English ->
            "Visibility"

        Spanish ->
            "Visibilidad"


weeklyForecast : Language -> String
weeklyForecast language =
    case language of
        English ->
            "Weekly Forecast"

        Spanish ->
            "Pronóstico Semanal"
