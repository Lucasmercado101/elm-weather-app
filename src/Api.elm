module Api exposing (..)

import Http
import Iso8601
import Json.Decode exposing (Decoder, field, float, int, list, map, map2, map3, map7, maybe, string)
import MIcons exposing (foggy, partlyCloudy, rainy, snowing, weatherSnowy)
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import Time exposing (Posix)
import Time.Extra exposing (Interval(..), add, posixToParts)
import Utils exposing (monthToInt)



-- REVERSE GEOCODING API


reverseGeocodingEndpoint : ( Float, Float ) -> String
reverseGeocodingEndpoint ( lat, long ) =
    "https://nominatim.openstreetmap.org/reverse?lat="
        ++ String.fromFloat lat
        ++ "&lon="
        ++ String.fromFloat long
        ++ "&format=json"
        ++ "&zoom=8"


type alias ReverseGeocodingResponse =
    { address : Address
    }


type alias Address =
    { country : String
    , state : String
    }


reverseGeocodingDecoder : Decoder ReverseGeocodingResponse
reverseGeocodingDecoder =
    map ReverseGeocodingResponse
        (field "address" addressDecoder)


addressDecoder : Decoder Address
addressDecoder =
    map2 Address
        (field "country" string)
        (field "state" string)


getReverseGeocoding : ( Float, Float ) -> (Result Http.Error ReverseGeocodingResponse -> msg) -> Cmd msg
getReverseGeocoding ( lat, long ) msg =
    Http.get
        { url =
            reverseGeocodingEndpoint ( lat, long )
        , expect =
            Http.expectJson msg
                reverseGeocodingDecoder
        }



-- WEATHER API


type alias EndpointQuery =
    { latitude : String
    , longitude : String
    , startDate : String
    , endDate : String
    }


endpoint : EndpointQuery -> String
endpoint { latitude, longitude, startDate, endDate } =
    -- TODO: more params dynamically, like timeZone
    "https://api.open-meteo.com/v1/forecast?latitude="
        ++ latitude
        ++ "&longitude="
        ++ longitude
        ++ "&hourly=temperature_2m,relativehumidity_2m,apparent_temperature,weathercode,visibility,windspeed_10m&daily=weathercode,temperature_2m_max&timezone=auto&start_date="
        ++ startDate
        ++ "&end_date="
        ++ endDate
        ++ ""


getData : ( Float, Float ) -> Posix -> Time.Zone -> (Result Http.Error ResponseData -> msg) -> Cmd msg
getData ( lat, long ) startDate zone msg =
    let
        latitude =
            String.fromFloat lat

        longitude =
            String.fromFloat long

        endDate =
            startDate |> add Day 7 zone

        dateToStr : Time.Extra.Parts -> String
        dateToStr parts =
            String.fromInt parts.year
                ++ "-"
                ++ (parts.month
                        |> monthToInt
                        |> String.fromInt
                        |> (\l ->
                                if String.length l == 1 then
                                    "0" ++ l

                                else
                                    l
                           )
                   )
                ++ "-"
                ++ (parts.day
                        |> String.fromInt
                        |> (\l ->
                                if String.length l == 1 then
                                    "0" ++ l

                                else
                                    l
                           )
                   )

        startDateString =
            startDate
                |> posixToParts zone
                |> dateToStr

        endDateStrings =
            endDate
                |> posixToParts zone
                |> dateToStr
    in
    Http.get
        { url =
            endpoint
                { latitude = latitude
                , longitude = longitude
                , startDate = startDateString
                , endDate = endDateStrings
                }
        , expect =
            Http.expectJson msg
                responseDataDecoder
        }


type alias ResponseData =
    { daily : Daily
    , hourly : List Hourly
    }



-- NOTE: normalize Hourly so that they all have a value
-- and ignore that have one missing?


type alias Hourly =
    { time : Posix
    , temperature : Maybe Float
    , relativeHumidity : Int
    , apparentTemperature : Maybe Float
    , weatherCode : Maybe WMOCode
    , windSpeed : Maybe Float
    , visibility : Float
    }


type alias MaxTemp =
    Float


type alias Daily =
    List ( Posix, WMOCode, MaxTemp )


dailyDecoder : Decoder Daily
dailyDecoder =
    map3
        (\times weatherCodes maxWeather ->
            let
                wmoCodes =
                    List.map codeToDescription weatherCodes
            in
            List.map3 (\t w m -> ( t, w, m )) times wmoCodes maxWeather
        )
        (field "time" (list string) |> map timesToPosix)
        (field "weathercode" (list int))
        (field "temperature_2m_max" (list float))


hourlyDecoder : Decoder (List Hourly)
hourlyDecoder =
    map7
        (\times temp humidity apparentTemp weatherCode windSpeed visibility ->
            listMap7
                (\time tem h at weatherC ws v ->
                    let
                        -- "let" here only for a better error message
                        hourly : Hourly
                        hourly =
                            { time = time
                            , temperature = tem
                            , relativeHumidity = h
                            , apparentTemperature = at
                            , weatherCode = weatherC |> Maybe.map codeToDescription
                            , windSpeed = ws
                            , visibility = v
                            }
                    in
                    hourly
                )
                times
                temp
                humidity
                apparentTemp
                weatherCode
                windSpeed
                visibility
        )
        (field "time" (list string) |> map timesToPosix)
        (field "temperature_2m" (list (maybe float)))
        (field "relativehumidity_2m" (list int))
        (field "apparent_temperature" (list (maybe float)))
        (field "weathercode" (list (maybe int)))
        (field "windspeed_10m" (list (maybe float)))
        (field "visibility" (list float))


responseDataDecoder : Decoder ResponseData
responseDataDecoder =
    map2 ResponseData
        (field "daily" dailyDecoder)
        (field "hourly" hourlyDecoder)


timesToPosix : List String -> List Posix
timesToPosix timesStr =
    List.map Iso8601.toTime timesStr
        -- Will never get here, if it does, it's the API's fault
        -- TODO: no .withDefault, handle any errors in the main view instead
        |> List.map (Result.withDefault (Time.millisToPosix 0))


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



-- WMO Weather interpretation codes (WW)


type WMOCode
    = ClearSky
    | MainlyClear
    | PartlyCloudy
    | Overcast
    | Fog
    | RimeFog
    | LightDrizzle
    | ModerateDrizzle
    | DenseDrizzle
    | LightFreezingDrizzle
    | DenseFreezingDrizzle
    | SlightRain
    | ModerateRain
    | HeavyRain
    | LightFreezingRain
    | HeavyFreezingRain
    | SlightSnowFall
    | ModerateSnowFall
    | HeavySnowFall
    | SnowGrains
    | SlightRainShowers
    | ModerateRainShowers
    | ViolentRainShowers
    | SlightSnowShowers
    | HeavySnowShowers
    | SlightOrModerateThunderstorm
    | SlightThunderstormWithHail
    | HeavyThunderstormWithHail


codeToDescription : Int -> WMOCode
codeToDescription code =
    case code of
        0 ->
            ClearSky

        1 ->
            MainlyClear

        2 ->
            MainlyClear

        3 ->
            Overcast

        45 ->
            Fog

        48 ->
            RimeFog

        51 ->
            LightDrizzle

        53 ->
            ModerateDrizzle

        55 ->
            DenseDrizzle

        56 ->
            LightFreezingDrizzle

        57 ->
            DenseFreezingDrizzle

        61 ->
            SlightRain

        63 ->
            ModerateRain

        65 ->
            HeavyRain

        66 ->
            LightFreezingRain

        67 ->
            HeavyFreezingRain

        71 ->
            SlightSnowFall

        73 ->
            ModerateSnowFall

        75 ->
            HeavySnowFall

        77 ->
            SnowGrains

        80 ->
            SlightRainShowers

        81 ->
            ModerateRainShowers

        82 ->
            ViolentRainShowers

        85 ->
            SlightSnowShowers

        86 ->
            HeavySnowShowers

        95 ->
            SlightOrModerateThunderstorm

        96 ->
            SlightThunderstormWithHail

        99 ->
            HeavyThunderstormWithHail

        _ ->
            ClearSky


wmoCodeToIcon : WMOCode -> Icon msg
wmoCodeToIcon code =
    case code of
        ClearSky ->
            Icons.wb_sunny

        MainlyClear ->
            Icons.wb_sunny

        PartlyCloudy ->
            partlyCloudy

        Overcast ->
            Icons.wb_cloudy

        Fog ->
            foggy

        RimeFog ->
            foggy

        LightDrizzle ->
            Icons.water_drop

        ModerateDrizzle ->
            Icons.water_drop

        DenseDrizzle ->
            Icons.water_drop

        LightFreezingDrizzle ->
            snowing

        DenseFreezingDrizzle ->
            snowing

        SlightRain ->
            rainy

        ModerateRain ->
            rainy

        HeavyRain ->
            rainy

        LightFreezingRain ->
            rainy

        HeavyFreezingRain ->
            rainy

        SlightSnowFall ->
            weatherSnowy

        ModerateSnowFall ->
            weatherSnowy

        HeavySnowFall ->
            weatherSnowy

        SnowGrains ->
            Icons.grain

        SlightRainShowers ->
            rainy

        ModerateRainShowers ->
            rainy

        ViolentRainShowers ->
            rainy

        SlightSnowShowers ->
            weatherSnowy

        HeavySnowShowers ->
            weatherSnowy

        SlightOrModerateThunderstorm ->
            Icons.thunderstorm

        SlightThunderstormWithHail ->
            Icons.thunderstorm

        HeavyThunderstormWithHail ->
            Icons.thunderstorm


wmoCodeToString : WMOCode -> String
wmoCodeToString code =
    case code of
        ClearSky ->
            "Clear sky"

        MainlyClear ->
            "Mainly clear"

        PartlyCloudy ->
            "Partly cloudy"

        Overcast ->
            "Overcast"

        Fog ->
            "Fog"

        RimeFog ->
            "Rime fog"

        LightDrizzle ->
            "Light drizzle"

        ModerateDrizzle ->
            "Moderate drizzle"

        DenseDrizzle ->
            "Dense drizzle"

        LightFreezingDrizzle ->
            "Light freezing drizzle"

        DenseFreezingDrizzle ->
            "Dense freezing drizzle"

        SlightRain ->
            "Slight rain"

        ModerateRain ->
            "Moderate rain"

        HeavyRain ->
            "Heavy rain"

        LightFreezingRain ->
            "Light freezing rain"

        HeavyFreezingRain ->
            "Heavy freezing rain"

        SlightSnowFall ->
            "Slight snow fall"

        ModerateSnowFall ->
            "Moderate snow fall"

        HeavySnowFall ->
            "Heavy snow fall"

        SnowGrains ->
            "Snow grains"

        SlightRainShowers ->
            "Slight rain showers"

        ModerateRainShowers ->
            "Moderate rain showers"

        ViolentRainShowers ->
            "Violent rain showers"

        SlightSnowShowers ->
            "Slight snow showers"

        HeavySnowShowers ->
            "Heavy snow showers"

        SlightOrModerateThunderstorm ->
            "Slight or moderate thunderstorm"

        SlightThunderstormWithHail ->
            "Slight thunderstorm with hail"

        HeavyThunderstormWithHail ->
            "Heavy thunderstorm with hail"
