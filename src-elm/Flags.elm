module Flags exposing (..)

import Api
import Element exposing (Color)
import Json.Decode exposing (..)
import Utils exposing (Theme)


type Flags
    = LanguageOnly
        { language : String
        }
    | CachedWeatherData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , usingGeoLocation : Bool
        , language : String
        , theme : Maybe Theme
        }
    | CachedWeatherAndAddressData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , country : String
        , state : Maybe String
        , city : Maybe String
        , usingGeoLocation : Bool
        , language : String
        , theme : Maybe Theme
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
    map
        (\language ->
            LanguageOnly
                { language = language
                }
        )
        (field "language" string)


cachedWeatherDataFlagDecoder : Decoder Flags
cachedWeatherDataFlagDecoder =
    map5
        (\time weatherData usingGeo language theme ->
            CachedWeatherData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , usingGeoLocation = usingGeo
                , language = language
                , theme = theme
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.responseDataDecoder)
        (field "usingGeoLocation" bool)
        (field "language" string)
        (maybe (field "theme" themeColorsDecoder))


cachedWeatherAndAddressDataDecoder : Decoder Flags
cachedWeatherAndAddressDataDecoder =
    map8
        (\time weatherData country maybeState maybeCity usingGeo language theme ->
            let
                ifEmptyThenNone : String -> Maybe String
                ifEmptyThenNone v =
                    if v == "" then
                        Nothing

                    else
                        Just v

                state : Maybe String
                state =
                    maybeState |> Maybe.andThen ifEmptyThenNone

                city : Maybe String
                city =
                    maybeCity |> Maybe.andThen ifEmptyThenNone
            in
            CachedWeatherAndAddressData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , country = country
                , state = state
                , usingGeoLocation = usingGeo
                , city = city
                , language = language
                , theme = theme
                }
        )
        (field "posixTimeNow" int)
        (field "cachedWeatherData" Api.responseDataDecoder)
        (field "country" string)
        (maybe (field "state" string))
        (maybe (field "city" string))
        (field "usingGeoLocation" bool)
        (field "language" string)
        (maybe (field "theme" themeColorsDecoder))


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
