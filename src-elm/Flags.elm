module Flags exposing (..)

import Api
import Element exposing (Color)
import Json.Decode exposing (..)
import List.Nonempty as NEList exposing (Nonempty)
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
        (maybe (field "customThemes" (list themeColorsDecoder)) |> map (Maybe.andThen NEList.fromList))


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
        (maybe (field "customThemes" (list themeColorsDecoder)) |> map (Maybe.andThen NEList.fromList))


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
