module Flags exposing (..)

import Api.Address
import Api.Weather
import Element exposing (Color)
import Json.Decode exposing (..)
import List.Nonempty exposing (Nonempty(..))
import Localizations exposing (Language(..))
import Ports
import Utils exposing (Theme)


type Flags
    = Initial Language
    | CachedWeatherData
        { posixTimeNow : Int
        , cachedWeatherData : Api.Weather.WeatherData
        , usingGeoLocation : Bool
        , language : Language
        , theme : Maybe Theme
        , customThemes : Maybe (Nonempty Theme)
        , timezone : String
        }
    | CachedWeatherAndAddressData
        { posixTimeNow : Int
        , cachedWeatherData : Api.Weather.WeatherData
        , addressData :
            { country : String
            , state : Maybe String
            , city : Maybe String
            }
        , usingGeoLocation : Bool
        , language : Language
        , theme : Maybe Theme
        , customThemes : Maybe (Nonempty Theme)
        , timezone : String
        }


langParse : String -> Language
langParse langStr =
    if langStr == "es" || String.contains "es-" langStr then
        Spanish

    else
        English


languageDecoder : Decoder Language
languageDecoder =
    oneOf
        [ int |> map Ports.langFromInt
        , string |> map langParse
        ]


colorDecoder : Decoder Color
colorDecoder =
    map3
        Element.rgb
        (field "r" float)
        (field "g" float)
        (field "b" float)


themeColorsDecoder : Decoder Theme
themeColorsDecoder =
    map2
        Tuple.pair
        (field "primary" colorDecoder)
        (field "secondary" colorDecoder)


{-| parses something like

```javascript
[ [ [0, 0, 0], [1,1,1] ], [ [1, 1, 1], [0, 0, 0] ] ]
```

into

```elm
Nonempty ( (0,0,0), (1,1,1) ) [ ( (1,1,1), (0,0,0) ) ]
```

-}
customThemeColorsDecoder : Decoder (Nonempty Theme)
customThemeColorsDecoder =
    list (list (list float))
        |> andThen
            (\l ->
                let
                    rgbToColor : List Float -> Maybe Color
                    rgbToColor colors =
                        case colors of
                            [ red, green, blue ] ->
                                Just (Element.rgb red green blue)

                            _ ->
                                Nothing

                    convertToTheme : List Float -> List Float -> Maybe Theme
                    convertToTheme primaryColors secondaryColors =
                        case ( rgbToColor primaryColors, rgbToColor secondaryColors ) of
                            ( Just primary, Just secondary ) ->
                                Just ( primary, secondary )

                            _ ->
                                Nothing

                    -- recursive
                    decodeValidThemes : List Theme -> List (List (List Float)) -> Decoder (Nonempty Theme)
                    decodeValidThemes acc nextColors =
                        case nextColors of
                            -- Base case: we have no more colors
                            [] ->
                                case acc of
                                    [] ->
                                        fail "No theme colors"

                                    -- We have a minimum of one theme
                                    x :: xs ->
                                        succeed (Nonempty x xs)

                            -- We may have at least one theme
                            possibleTheme :: rest ->
                                case possibleTheme of
                                    [ primaryColorArr, secondaryColorArr ] ->
                                        case convertToTheme primaryColorArr secondaryColorArr of
                                            Just validTheme ->
                                                decodeValidThemes (acc ++ [ validTheme ]) rest

                                            Nothing ->
                                                fail "Invalid custom theme colors, either missing RGB values or invalid RGB values"

                                    _ ->
                                        fail "Invalid Theme color"
                in
                decodeValidThemes [] l
            )


initialFlagDecoder : Decoder Flags
initialFlagDecoder =
    languageDecoder |> map Initial


jsonField : String -> Decoder a -> Decoder a
jsonField fieldName decoder =
    field fieldName string
        |> map (decodeString decoder)
        |> andThen
            (\result ->
                case result of
                    Ok value ->
                        succeed value

                    Err _ ->
                        fail ("Failed to decode " ++ fieldName ++ " field")
            )


cachedWeatherDataFlagDecoder : Decoder Flags
cachedWeatherDataFlagDecoder =
    map7
        (\time weatherData usingGeo language theme customThemes timezone ->
            CachedWeatherData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , usingGeoLocation = usingGeo
                , language = language
                , theme = theme
                , customThemes = customThemes
                , timezone = timezone
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.Weather.responseDataDecoder)
        (maybe (jsonField "usingGeoLocation" bool) |> map (Maybe.withDefault False))
        (field "language" languageDecoder)
        (maybe (field "theme" themeColorsDecoder))
        (maybe (jsonField "customThemes" customThemeColorsDecoder))
        (field "timezone" string)


toNonEmptyStr : String -> Maybe String
toNonEmptyStr v =
    if v == "" then
        Nothing

    else
        Just v


cachedAddressDataDecoder : Decoder Api.Address.Address
cachedAddressDataDecoder =
    map3 Api.Address.Address
        (field "country" string)
        (maybe (field "state" string) |> map (Maybe.andThen toNonEmptyStr))
        (maybe (field "city" string) |> map (Maybe.andThen toNonEmptyStr))


cachedWeatherAndAddressDataDecoder : Decoder Flags
cachedWeatherAndAddressDataDecoder =
    map8
        (\time weatherData addressData usingGeo language theme customThemes timezone ->
            CachedWeatherAndAddressData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , addressData = addressData
                , usingGeoLocation = usingGeo
                , language = language
                , theme = theme
                , customThemes = customThemes
                , timezone = timezone
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.Weather.responseDataDecoder)
        (field "addressData" cachedAddressDataDecoder)
        (maybe (jsonField "usingGeoLocation" bool) |> map (Maybe.withDefault False))
        (field "language" languageDecoder)
        (maybe (field "theme" themeColorsDecoder))
        (maybe (jsonField "customThemes" customThemeColorsDecoder))
        (field "timezone" string)


flagsDecoders : Value -> Result Error Flags
flagsDecoders value =
    decodeValue
        (oneOf
            [ cachedWeatherAndAddressDataDecoder
            , cachedWeatherDataFlagDecoder
            , initialFlagDecoder
            ]
        )
        value
