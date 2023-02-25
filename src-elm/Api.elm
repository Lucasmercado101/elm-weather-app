module Api exposing (..)

import Http
import Iso8601
import Json.Decode exposing (Decoder, decodeString, field, float, int, list, map, map3, map4, map7, maybe, string)
import MIcons exposing (foggy, partlyCloudy, rainy, snowing, weatherSnowy)
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import Task exposing (Task)
import Time exposing (Posix, Zone)
import Time.Extra exposing (Interval(..), add, posixToParts)
import Utils exposing (Coordinates, monthToInt)



-- REVERSE GEOCODING API


reverseGeocodingEndpoint : Coordinates -> String
reverseGeocodingEndpoint { latitude, longitude } =
    "https://nominatim.openstreetmap.org/reverse?lat="
        ++ String.fromFloat latitude
        ++ "&lon="
        ++ String.fromFloat longitude
        ++ "&format=json"
        ++ "&zoom=8"


type alias ReverseGeocodingResponse =
    { address : Address
    }


type alias Address =
    { country : String
    , state : Maybe String
    , city : Maybe String
    }


reverseGeocodingDecoder : Decoder ReverseGeocodingResponse
reverseGeocodingDecoder =
    map ReverseGeocodingResponse
        (field "address" addressDecoder)


addressDecoder : Decoder Address
addressDecoder =
    map3
        (\country maybeState maybeCity ->
            let
                ifEmptyThenNone v =
                    if v == "" then
                        Nothing

                    else
                        Just v

                state =
                    maybeState |> Maybe.andThen ifEmptyThenNone

                city =
                    maybeCity |> Maybe.andThen ifEmptyThenNone
            in
            { country = country
            , state = state
            , city = city
            }
        )
        (field "country" string)
        (maybe (field "state" string))
        (maybe (field "city" string))


getReverseGeocoding : Coordinates -> (Result Http.Error ReverseGeocodingResponse -> msg) -> Cmd msg
getReverseGeocoding coordinates msg =
    Http.get
        { url = reverseGeocodingEndpoint coordinates
        , expect = Http.expectJson msg reverseGeocodingDecoder
        }



-- WEATHER API


type alias EndpointQuery =
    { latitude : String
    , longitude : String
    , startDate : String
    , endDate : String
    }


getWeatherDataEndpoint : EndpointQuery -> String
getWeatherDataEndpoint { latitude, longitude, startDate, endDate } =
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


getWeatherDataAsTask : Coordinates -> Task Http.Error ( ResponseData, Posix, Zone )
getWeatherDataAsTask { latitude, longitude } =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.andThen
            (\( zone, posix ) ->
                let
                    startDate =
                        posix

                    lat =
                        String.fromFloat latitude

                    lon =
                        String.fromFloat longitude

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
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = getWeatherDataEndpoint { latitude = lat, longitude = lon, startDate = startDateString, endDate = endDateStrings }
                    , body = Http.emptyBody
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.BadUrl_ url ->
                                        Err (Http.BadUrl url)

                                    Http.Timeout_ ->
                                        Err Http.Timeout

                                    Http.NetworkError_ ->
                                        Err Http.NetworkError

                                    Http.BadStatus_ metadata body ->
                                        Err (Http.BadStatus metadata.statusCode)

                                    Http.GoodStatus_ metadata body ->
                                        case decodeString responseDataDecoder body of
                                            Ok value ->
                                                Ok ( value, posix, zone )

                                            Err err ->
                                                Err (Http.BadBody (Json.Decode.errorToString err))
                            )
                    , timeout = Just 10000
                    }
            )


getWeatherData : Coordinates -> (Result Http.Error ( ResponseData, Posix, Zone ) -> msg) -> Cmd msg
getWeatherData coords msg =
    getWeatherDataAsTask coords |> Task.attempt msg


type alias ResponseData =
    { daily : Daily
    , hourly : List Hourly
    , latitude : Float
    , longitude : Float
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
    map4 ResponseData
        (field "daily" dailyDecoder)
        (field "hourly" hourlyDecoder)
        (field "latitude" float)
        (field "longitude" float)


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


esWmoCodeToString : WMOCode -> String
esWmoCodeToString code =
    case code of
        ClearSky ->
            "Cielo despejado"

        MainlyClear ->
            "Mayormente despejado"

        PartlyCloudy ->
            "Parcialmente nublado"

        Overcast ->
            "Nublado"

        Fog ->
            "Niebla"

        RimeFog ->
            "Niebla helada"

        LightDrizzle ->
            "Llovizna ligera"

        ModerateDrizzle ->
            "Llovizna moderada"

        DenseDrizzle ->
            "Llovizna densa"

        LightFreezingDrizzle ->
            "Llovizna ligera helada"

        DenseFreezingDrizzle ->
            "Llovizna densa helada"

        SlightRain ->
            "Lluvia ligera"

        ModerateRain ->
            "Lluvia moderada"

        HeavyRain ->
            "Lluvia intensa"

        LightFreezingRain ->
            "Lluvia ligera helada"

        HeavyFreezingRain ->
            "Lluvia intensa helada"

        SlightSnowFall ->
            "Nevada ligera"

        ModerateSnowFall ->
            "Nevada moderada"

        HeavySnowFall ->
            "Nevada intensa"

        SnowGrains ->
            "Copos de nieve"

        SlightRainShowers ->
            "Lluvia ligera con chubascos"

        ModerateRainShowers ->
            "Lluvia moderada con chubascos"

        ViolentRainShowers ->
            "Lluvia intensa con chubascos"

        SlightSnowShowers ->
            "Nevada ligera con chubascos"

        HeavySnowShowers ->
            "Nevada intensa con chubascos"

        SlightOrModerateThunderstorm ->
            "Tormenta ligera o moderada"

        SlightThunderstormWithHail ->
            "Tormenta ligera con granizo"

        HeavyThunderstormWithHail ->
            "Tormenta intensa con granizo"
