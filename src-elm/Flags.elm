module Flags exposing (..)

import Api
import Element exposing (Color)
import Json.Decode exposing (..)
import List.Nonempty as NEList exposing (Nonempty(..))
import Ports
import Utils exposing (Theme)


type Flags
    = LanguageOnly String
    | CachedWeatherData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , usingGeoLocation : Bool
        , language : String
        , theme : Maybe Theme
        , customThemes : Maybe (Nonempty Theme)
        }
    | CachedWeatherAndAddressData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , addressData :
            { country : String
            , state : Maybe String
            , city : Maybe String
            }
        , usingGeoLocation : Bool
        , language : String
        , theme : Maybe Theme
        , customThemes : Maybe (Nonempty Theme)
        }


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


customThemeColorsDecoder : Decoder (Nonempty Theme)
customThemeColorsDecoder =
    list (list (list float))
        |> andThen
            (\l ->
                let
                    isThemeColor : List Float -> Maybe Color
                    isThemeColor colors =
                        case colors of
                            [ red, green, blue ] ->
                                Just (Element.rgb red green blue)

                            _ ->
                                Nothing

                    isValidTheme : List Float -> List Float -> Maybe Theme
                    isValidTheme primaryColors secondaryColors =
                        case ( isThemeColor primaryColors, isThemeColor secondaryColors ) of
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

                            -- We have at least one theme
                            possibleTheme :: restColors ->
                                case possibleTheme of
                                    [ primaryColorArr, secondaryColorArr ] ->
                                        case isValidTheme primaryColorArr secondaryColorArr of
                                            Just themeColor ->
                                                decodeValidThemes (acc ++ [ themeColor ]) restColors

                                            Nothing ->
                                                fail "Invalid custom theme colors, either missing RGB values or invalid RGB values"

                                    _ ->
                                        fail "Invalid Theme color"
                in
                decodeValidThemes [] l
            )


languageOnlyDecoder : Decoder Flags
languageOnlyDecoder =
    string |> map LanguageOnly


cachedWeatherDataFlagDecoder : Decoder Flags
cachedWeatherDataFlagDecoder =
    map6
        (\time weatherData usingGeo language theme customThemes ->
            CachedWeatherData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , usingGeoLocation = usingGeo
                , language = language
                , theme = theme
                , customThemes = customThemes
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.responseDataDecoder)
        (field "usingGeoLocation" bool)
        (field "language" string)
        (maybe (field "theme" themeColorsDecoder))
        (maybe (field "customThemes" customThemeColorsDecoder))


cachedWeatherAndAddressDataDecoder : Decoder Flags
cachedWeatherAndAddressDataDecoder =
    let
        ifEmptyThenNone : String -> Maybe String
        ifEmptyThenNone v =
            if v == "" then
                Nothing

            else
                Just v
    in
    map7
        (\time weatherData addressData usingGeo language theme customThemes ->
            CachedWeatherAndAddressData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , addressData = addressData
                , usingGeoLocation = usingGeo
                , language = language
                , theme = theme
                , customThemes = customThemes
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.responseDataDecoder)
        (field "addressData"
            (map3 (\country state city -> { country = country, state = state, city = city })
                (field "country" string)
                (maybe (field "state" string) |> map (Maybe.andThen ifEmptyThenNone))
                (maybe (field "city" string) |> map (Maybe.andThen ifEmptyThenNone))
            )
        )
        (field "usingGeoLocation" bool)
        (field "language" string)
        (maybe (field "theme" themeColorsDecoder))
        (maybe (field "customThemes" customThemeColorsDecoder))


flagsDecoders : Value -> Result Error Flags
flagsDecoders value =
    decodeValue
        (oneOf
            [ cachedWeatherAndAddressDataDecoder
            , cachedWeatherDataFlagDecoder
            , languageOnlyDecoder
            ]
        )
        value
