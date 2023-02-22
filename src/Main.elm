module Main exposing (..)

import Animator
import Api exposing (Hourly, ResponseData, ReverseGeocodingResponse, WMOCode, wmoCodeToIcon, wmoCodeToString)
import Browser
import Cmd.Extra exposing (pure)
import Element exposing (Color, Element, alpha, centerX, centerY, column, el, fill, height, inFront, layout, link, none, padding, paddingEach, paddingXY, paragraph, px, rgb, rotate, row, scrollbarX, spaceEvenly, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as JD
import List.Nonempty as NEList exposing (Nonempty(..))
import MIcons exposing (..)
import Material.Icons as Icons
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..), Icon)
import ParseInt
import Regex
import Screens.Welcome as Welcome
import Task
import Time exposing (Posix, Zone)
import Utils exposing (..)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        MainScreen modelData ->
            animator
                |> Animator.toSubscription (\l -> OnMainScreenMsg (Tick l)) modelData
                |> Sub.map (\l -> l)

        LoadingScreen _ ->
            Sub.none

        WelcomeScreen m ->
            Welcome.welcomeScreenSubscriptions m
                |> Sub.map OnWelcomeScreenMsg



-- MODEL


type RefetchingStatus a
    = NotRefetching
    | Refetching
    | Error a


animator : Animator.Animator MainScreenModel
animator =
    Animator.animator
        |> Animator.watching
            .currentRefetchingAnim
            (\newChecked model ->
                { model | currentRefetchingAnim = newChecked }
            )
        |> Animator.watching
            .countryAndStateVisibility
            (\newVisibility model ->
                { model | countryAndStateVisibility = newVisibility }
            )


type alias MainScreenModel =
    { currentRefetchingAnim : Animator.Timeline (RefetchingStatus Http.Error)
    , currentRefetchingStatus : RefetchingStatus Http.Error
    , location : Coordinates
    , primaryColor : Color
    , isOptionMenuOpen : Bool
    , zone : Maybe Zone

    -- NOTE: could be fetched before on the loading screen
    -- but if the user is mainly on the MainScreen + caching then it's
    -- unnecessary overhead and logic that's not worth it
    -- NOTE: when I fetch I return response and current time posix
    -- they're synced as I don't need to use posix anywhere else
    -- but when I get the data and to do things at the time I fetched it
    , apiData : ( ResponseData, Posix )

    -- NOTE: could be made into a Loading | Loaded | Error type union
    -- can't be bothered though
    , country : String
    , state : String
    , countryAndStateVisibility : Animator.Timeline Bool
    }


type alias LoadingScreenModel =
    ( InitialWebRequest Http.Error, Coordinates )


type Model
    = WelcomeScreen Welcome.WelcomeScreenModel
    | LoadingScreen LoadingScreenModel
    | MainScreen MainScreenModel



-- MESSAGE


type LoadingScreenMsg
    = RetryFetchingWeather
    | GotWeatherResponse (Result Http.Error ( ResponseData, Posix, Zone ))


type MainScreenMsg
    = ChangedPrimaryColor String
    | OpenOptionsMenu
    | CloseOptionsMenu
    | RefetchWeatherOnBackground
    | GotRefetchingWeatherResp (Result Http.Error ( ResponseData, Posix, Zone ))
    | Tick Time.Posix
    | GotCountryAndStateMainScreen (Result Http.Error ReverseGeocodingResponse)


type Msg
    = OnWelcomeScreenMsg Welcome.WelcomeScreenMsg
    | OnLoadingScreenMsg LoadingScreenMsg
    | OnMainScreenMsg MainScreenMsg



-- MAIN
-- TODO: handle receiving error on
-- location: granted but error on attempting to get location


type Flags
    = GeoLocationCoords Coordinates
    | CachedWeatherData { posixTimeNow : Int, cachedWeatherData : Api.ResponseData }


geoLocationsCoordsFlagDecoder : JD.Decoder Flags
geoLocationsCoordsFlagDecoder =
    JD.map2
        (\latitude longitude ->
            GeoLocationCoords
                { latitude = latitude
                , longitude = longitude
                }
        )
        (JD.field "latitude" JD.float)
        (JD.field "longitude" JD.float)


cachedWeatherDataFlagDecoder : JD.Decoder Flags
cachedWeatherDataFlagDecoder =
    JD.map2
        (\time weatherData ->
            CachedWeatherData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                }
        )
        (JD.field "posixTimeNow" JD.int)
        (JD.field "cachedWeatherData" Api.responseDataDecoder)


flagsDecoders : JD.Value -> Result JD.Error Flags
flagsDecoders value =
    JD.decodeValue
        (JD.oneOf
            [ cachedWeatherDataFlagDecoder
            , geoLocationsCoordsFlagDecoder
            ]
        )
        value


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TODO: remove from cache when location perms are revoked
-- otherwise i have an old locations


init : JD.Value -> ( Model, Cmd Msg )
init val =
    case flagsDecoders val of
        Ok flags ->
            case flags of
                -- NOTE: Rare: realistically the data would've already
                -- been fetched as soon as coordinates were given
                GeoLocationCoords coords ->
                    ( LoadingScreen ( Loading, coords )
                    , Api.getWeatherData coords
                        |> Task.attempt (\l -> OnLoadingScreenMsg (GotWeatherResponse l))
                    )

                CachedWeatherData { cachedWeatherData, posixTimeNow } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData
                    in
                    ( MainScreen
                        { apiData = ( cachedWeatherData, Time.millisToPosix posixTimeNow )
                        , currentRefetchingStatus = NotRefetching
                        , currentRefetchingAnim = Animator.init NotRefetching
                        , location = { latitude = latitude, longitude = longitude }
                        , primaryColor = primary
                        , isOptionMenuOpen = False
                        , country = ""
                        , state = ""
                        , countryAndStateVisibility = Animator.init False

                        -- TODO: handle zone, when refreshing there's no good initial value
                        -- TODO: send zone when I have it and cache it
                        -- and get it from init https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/TimeZone#zones
                        , zone = Just Time.utc
                        }
                    , Cmd.batch
                        [ Api.getWeatherData { latitude = cachedWeatherData.latitude, longitude = cachedWeatherData.longitude }
                            |> Task.attempt (\l -> OnMainScreenMsg (GotRefetchingWeatherResp l))
                        , Api.getReverseGeocoding { latitude = latitude, longitude = longitude } GotCountryAndStateMainScreen
                            |> Cmd.map OnMainScreenMsg
                        ]
                    )

        Err _ ->
            -- NOTE: this will not happen unless i screw up the flags
            WelcomeScreen Welcome.welcomeScreenInit |> pure



-- UPDATE


mapToMainScreen : ( MainScreenModel, Cmd MainScreenMsg ) -> ( Model, Cmd Msg )
mapToMainScreen ( a, b ) =
    ( MainScreen a, Cmd.map OnMainScreenMsg b )


update : Msg -> Model -> ( Model, Cmd Msg )
update topMsg topModel =
    case ( topMsg, topModel ) of
        ( OnWelcomeScreenMsg msg, WelcomeScreen model ) ->
            Welcome.welcomeScreenUpdate msg model
                |> (\( a, b ) ->
                        case a.receivedLocation of
                            Just coords ->
                                ( LoadingScreen ( Loading, coords )
                                , Api.getWeatherData coords
                                    |> Task.attempt (\l -> OnLoadingScreenMsg (GotWeatherResponse l))
                                )

                            Nothing ->
                                ( WelcomeScreen a, Cmd.map OnWelcomeScreenMsg b )
                   )

        ( OnLoadingScreenMsg msg, (LoadingScreen ( state, coords )) as model ) ->
            -- NOTE: could probably be refactored?
            -- doesn't seem to be worth it though
            case msg of
                GotWeatherResponse result ->
                    case result of
                        Ok ( data, currentTime, zone ) ->
                            ( MainScreen
                                { apiData = ( data, currentTime )
                                , currentRefetchingStatus = NotRefetching
                                , currentRefetchingAnim = Animator.init NotRefetching
                                , location = { latitude = data.latitude, longitude = data.longitude }
                                , zone = Just zone
                                , primaryColor = primary
                                , isOptionMenuOpen = False
                                , country = ""
                                , state = ""
                                , countryAndStateVisibility = Animator.init False
                                }
                            , Api.getReverseGeocoding { latitude = data.latitude, longitude = data.longitude } GotCountryAndStateMainScreen
                                |> Cmd.map OnMainScreenMsg
                            )

                        Err err ->
                            LoadingScreen ( Failure err, coords ) |> pure

                RetryFetchingWeather ->
                    ( LoadingScreen ( Loading, coords )
                    , Api.getWeatherData coords
                        |> Task.attempt (\l -> OnLoadingScreenMsg (GotWeatherResponse l))
                    )

        ( OnMainScreenMsg msg, MainScreen model ) ->
            case msg of
                Tick newTime ->
                    (model |> Animator.update newTime animator)
                        |> pure
                        |> mapToMainScreen

                ChangedPrimaryColor hexColor ->
                    { model | primaryColor = hexColor |> hexToColor |> Result.withDefault model.primaryColor }
                        |> pure
                        |> mapToMainScreen

                OpenOptionsMenu ->
                    { model | isOptionMenuOpen = True }
                        |> pure
                        |> mapToMainScreen

                CloseOptionsMenu ->
                    { model | isOptionMenuOpen = False }
                        |> pure
                        |> mapToMainScreen

                GotCountryAndStateMainScreen countryAndState ->
                    case countryAndState of
                        Ok { address } ->
                            { model
                                | country = address.country
                                , state = address.state
                                , countryAndStateVisibility =
                                    model.countryAndStateVisibility
                                        |> Animator.go Animator.slowly True
                            }
                                |> pure
                                |> mapToMainScreen

                        Err _ ->
                            -- NOTE: not handling error on purpose
                            { model
                                | country = ""
                                , state = ""
                                , countryAndStateVisibility =
                                    model.countryAndStateVisibility
                                        |> Animator.go Animator.slowly False
                            }
                                |> pure
                                |> mapToMainScreen

                RefetchWeatherOnBackground ->
                    ( { model
                        | currentRefetchingAnim =
                            model.currentRefetchingAnim
                                |> Animator.go Animator.immediately Refetching
                        , currentRefetchingStatus = Refetching
                      }
                    , Cmd.batch
                        [ Api.getReverseGeocoding model.location (\l -> GotCountryAndStateMainScreen l)

                        --  TODO: currently refetching using given coordinates,
                        --  but it should be the coordinates given if no locations perms allowed
                        --  otherwise get current location and fetch using that, also add option to change
                        --  location on menu
                        , Api.getWeatherData model.location
                            |> Task.attempt (\l -> GotRefetchingWeatherResp l)
                        ]
                    )
                        |> mapToMainScreen

                GotRefetchingWeatherResp result ->
                    (case result of
                        Ok ( data, posix, zone ) ->
                            { model
                                | apiData = ( data, posix )
                                , currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately NotRefetching
                                , currentRefetchingStatus = NotRefetching
                                , zone = Just zone
                            }

                        Err err ->
                            { model
                                | currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately (Error err)
                                , currentRefetchingStatus = Error err
                            }
                    )
                        |> pure
                        |> mapToMainScreen

        ( _, _ ) ->
            topModel |> pure



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ Font.family
            [ Font.typeface "Inter"
            , Font.sansSerif
            ]
        , inFront
            (case model of
                MainScreen modelData ->
                    let
                        divider =
                            el [ width fill, height (px 1), Background.color modelData.primaryColor ] none
                    in
                    if modelData.isOptionMenuOpen then
                        column
                            [ width fill
                            , Background.color black
                            ]
                            [ row
                                [ width fill
                                , paddingXY 15 8
                                ]
                                [ el
                                    [ width fill
                                    , Font.color modelData.primaryColor
                                    , Font.heavy
                                    ]
                                    (text "Primary Color")
                                , Html.input
                                    [ Html.Attributes.type_ "color"
                                    , Html.Attributes.style "border" "transparent"
                                    , Html.Attributes.style "background" "transparent"
                                    , Html.Attributes.style "height" "35px"
                                    , Html.Attributes.value
                                        (toRgb modelData.primaryColor
                                            |> (\{ blue, green, red } ->
                                                    List.map toHex
                                                        [ round (red * 255)
                                                        , round (green * 255)
                                                        , round (blue * 255)

                                                        -- don't know how to do alpha so i'm just omitting it here
                                                        -- , round (alpha * 255)
                                                        ]
                                                        |> (::) "#"
                                                        |> String.join ""
                                               )
                                        )
                                    , Html.Attributes.style "width" "35px"
                                    , Html.Events.onInput (\l -> OnMainScreenMsg (ChangedPrimaryColor l))
                                    ]
                                    []
                                    |> Element.html
                                ]
                            , divider
                            , row [ width fill ]
                                [ button
                                    [ width fill
                                    , Font.color modelData.primaryColor
                                    , padding 15
                                    , Font.heavy
                                    , Font.center
                                    ]
                                    { label = text "X", onPress = Just (OnMainScreenMsg CloseOptionsMenu) }
                                ]
                            , divider
                            ]

                    else
                        none

                LoadingScreen _ ->
                    none

                WelcomeScreen noLocationDataModel ->
                    if noLocationDataModel.geoLocationApiError /= "" then
                        paragraph
                            [ Background.color black
                            , Font.color primary
                            , Font.bold
                            , paddingXY 24 12
                            , Font.size 22
                            ]
                            [ text "Error: ", el [ Font.light ] (text noLocationDataModel.geoLocationApiError) ]

                    else
                        none
            )
        ]
        (case model of
            WelcomeScreen m ->
                Welcome.welcomeScreenView m
                    |> Element.map OnWelcomeScreenMsg

            LoadingScreen m ->
                loadingScreenView m

            MainScreen m ->
                mainScreen m |> Element.map OnMainScreenMsg
        )


loadingScreenView : LoadingScreenModel -> Element Msg
loadingScreenView ( state, _ ) =
    el
        [ width fill
        , height fill
        ]
        (el
            [ width fill
            , height fill
            , Background.color primary
            , paddingEach { top = 15, bottom = 16, left = 0, right = 0 }
            ]
            (case state of
                Loading ->
                    initialLoadingScreen

                Failure err ->
                    column [ centerX, centerY ]
                        [ paragraph [ Font.center, Font.size 54, Font.semiBold ]
                            [ el [ Font.center ] (text "Something went wrong:")
                            , br
                            , el [ Font.heavy ]
                                (case err of
                                    Http.BadUrl url ->
                                        text ("Bad URL: " ++ url)

                                    Http.Timeout ->
                                        text "Timeout"

                                    Http.NetworkError ->
                                        text "Network Error"

                                    Http.BadStatus response ->
                                        text ("Bad Status: " ++ String.fromInt response)

                                    Http.BadBody _ ->
                                        text "Error parsing body"
                                )
                            ]
                        , el [ paddingTop 18, centerX ]
                            (button
                                [ centerX
                                , Background.color black
                                , Font.color white
                                , Font.bold
                                , paddingXY 24 12
                                , Font.size 22
                                ]
                                { label = text "RETRY", onPress = Just (OnLoadingScreenMsg RetryFetchingWeather) }
                            )
                        ]
            )
        )


mainScreen : MainScreenModel -> Element MainScreenMsg
mainScreen model =
    let
        -- NOTE: this is just accounting for the brief moment
        -- if and when I don't have the user's time zone
        zone =
            Maybe.withDefault Time.utc model.zone

        ( apiData, currentTime ) =
            model.apiData

        currentDateChip : Element MainScreenMsg
        currentDateChip =
            el
                [ centerX
                , Background.color black
                , paddingXY 16 8
                , Border.rounded 200
                ]
                (paragraph [ Font.color model.primaryColor, Font.size 14, Font.light ]
                    [ text
                        (currentTime
                            |> Time.toWeekday zone
                            |> dayToString
                        )
                    , text ", "
                    , text
                        (currentTime
                            |> Time.toDay zone
                            |> String.fromInt
                        )
                    , text " "
                    , text
                        (currentTime
                            |> Time.toMonth zone
                            |> monthToString
                        )
                    ]
                )

        dailySummary : Element MainScreenMsg
        dailySummary =
            -- NOTE: could be even more strict and only a custom list type with 24
            -- list items, i.e the hours in a day, each being an Hourly,
            -- definitely overkill.
            case hourlyDataOfToday zone currentTime apiData.hourly |> NEList.fromList of
                Just ((Nonempty firstHourly restHourly) as todayHourlyData) ->
                    column []
                        [ el
                            [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                            , Font.heavy
                            ]
                            (text "Daily summary")
                        , let
                            closestHourly : Hourly
                            closestHourly =
                                timeClosestToMine zone currentTime firstHourly restHourly

                            allTemperaturesOfToday : Nonempty (Maybe Float)
                            allTemperaturesOfToday =
                                todayHourlyData
                                    |> NEList.map .temperature

                            lowestTempOfToday : Maybe Float
                            lowestTempOfToday =
                                allTemperaturesOfToday
                                    |> (\(Nonempty first rest) ->
                                            first |> Maybe.map (\num -> foldrMaybeListWithDefault num rest min)
                                       )

                            highestTempOfToday : Maybe Float
                            highestTempOfToday =
                                allTemperaturesOfToday
                                    |> (\(Nonempty first rest) ->
                                            first |> Maybe.map (\num -> foldrMaybeListWithDefault num rest max)
                                       )
                          in
                          paragraph
                            [ paddingEach { top = 14, left = 15, right = 0, bottom = 0 }
                            , Font.bold
                            , Font.size 16
                            , width fill
                            ]
                            [ case ( closestHourly.apparentTemperature, closestHourly.temperature ) of
                                ( Just apparent, Just actual ) ->
                                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°, it's actually " ++ (actual |> String.fromFloat) ++ "°")

                                ( Just apparent, Nothing ) ->
                                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°")

                                ( Nothing, Just actual ) ->
                                    text ("Now it's " ++ (actual |> String.fromFloat) ++ "°")

                                ( Nothing, Nothing ) ->
                                    -- NOTE: in theory should never happen
                                    none
                            , br
                            , case ( lowestTempOfToday, highestTempOfToday ) of
                                ( Just lowest, Just highest ) ->
                                    text ("Today, the temperature is felt in the range from " ++ (lowest |> String.fromFloat) ++ "° to " ++ (highest |> String.fromFloat) ++ "°")

                                ( Just lowest, Nothing ) ->
                                    text ("Today, the temperature lowest temperature is " ++ (lowest |> String.fromFloat) ++ "°")

                                ( Nothing, Just highest ) ->
                                    text ("Today, the temperature highest temperature is " ++ (highest |> String.fromFloat) ++ "°")

                                ( Nothing, Nothing ) ->
                                    none
                            ]
                        ]

                _ ->
                    none

        statCards : Element MainScreenMsg
        statCards =
            let
                windCard v =
                    statCard model.primaryColor Icons.air "Wind" v

                humidityCard v =
                    statCard model.primaryColor Icons.water_drop "Humidity" v

                visibilityCard v =
                    statCard model.primaryColor Icons.visibility "Visibility" v
            in
            el
                [ padding 15
                , width fill
                ]
                (row
                    [ Font.color white
                    , Background.color black
                    , width fill
                    , padding 30
                    , Border.rounded 12
                    , spaceEvenly
                    ]
                    (case apiData.hourly |> hourlyDataOfToday zone currentTime |> NEList.fromList of
                        Just (Nonempty firstHourly restHourly) ->
                            let
                                hourlyClosestToMine =
                                    timeClosestToMine zone currentTime firstHourly restHourly
                            in
                            [ windCard
                                (hourlyClosestToMine
                                    |> .windSpeed
                                    -- NOTE: it can come as null in the JSON
                                    |> Maybe.map
                                        (\l ->
                                            l
                                                |> round
                                                |> String.fromInt
                                                |> (\s -> s ++ "km/h")
                                        )
                                    |> Maybe.withDefault "--"
                                )
                            , humidityCard
                                ((hourlyClosestToMine
                                    |> .relativeHumidity
                                    |> String.fromInt
                                 )
                                    ++ "%"
                                )
                            , visibilityCard
                                ((hourlyClosestToMine
                                    |> .visibility
                                    |> toKm
                                    |> round
                                    |> String.fromInt
                                 )
                                    ++ "km"
                                )
                            ]

                        Nothing ->
                            [ -- NOTE: in theory it will never reach here
                              -- as there will always be one item in the list
                              -- either way it's handled as "--" in all 3 stat cards
                              windCard "--"
                            , humidityCard "--"
                            , visibilityCard "--"
                            ]
                    )
                )
    in
    el
        [ width fill
        , height fill
        , Background.color model.primaryColor
        , paddingEach { top = 15, bottom = 16, left = 0, right = 0 }
        ]
        (column
            [ width fill
            , height fill
            ]
            [ row [ width fill, paddingBottom 15 ]
                [ button
                    [ padding 15
                    , Font.color black
                    , Font.heavy
                    , Font.center
                    ]
                    { label =
                        el
                            [ rotate
                                (Animator.move model.currentRefetchingAnim <|
                                    \state ->
                                        case state of
                                            NotRefetching ->
                                                Animator.at 0

                                            Refetching ->
                                                Animator.wrap 0 (2 * pi)
                                                    |> Animator.loop (Animator.millis 800)

                                            Error _ ->
                                                Animator.at 0
                                )
                            ]
                            (case model.currentRefetchingStatus of
                                NotRefetching ->
                                    Icons.refresh 28 Inherit
                                        |> Element.html

                                Refetching ->
                                    Icons.hourglass_empty 28 Inherit
                                        |> Element.html

                                Error _ ->
                                    Icons.sync_problem 28 Inherit
                                        |> Element.html
                            )
                    , onPress = Just RefetchWeatherOnBackground
                    }
                , column
                    [ width fill
                    , spacing 6
                    , alpha
                        (Animator.move model.countryAndStateVisibility <|
                            \state ->
                                if state == True then
                                    Animator.at 1

                                else
                                    Animator.at 0
                        )
                    ]
                    [ paragraph
                        [ centerX
                        , centerY
                        , Font.heavy
                        , Font.size 28
                        , Font.center
                        ]
                        [ text model.country ]
                    , paragraph
                        [ centerX
                        , centerY
                        , Font.center
                        ]
                        [ text model.state ]
                    ]
                , button
                    [ padding 15
                    , Font.color black
                    , Font.heavy
                    , Font.center
                    ]
                    { label = Icons.menu 28 Inherit |> Element.html, onPress = Just OpenOptionsMenu }
                ]
            , currentDateChip
            , case apiData.hourly of
                x :: xs ->
                    let
                        closestHourly : Hourly
                        closestHourly =
                            timeClosestToMine zone currentTime x xs

                        actualTemp : String
                        actualTemp =
                            case closestHourly.temperature of
                                Just val ->
                                    val |> round |> String.fromInt

                                Nothing ->
                                    -- NOTE: in theory this will never happen
                                    -- as we know what temperature it is "right now"
                                    "--"
                    in
                    column [ width fill ]
                        [ el [ width fill, Font.center, Font.bold, paddingEach { top = 14, bottom = 12, left = 0, right = 0 } ]
                            (text
                                (wmoCodeToString
                                    (closestHourly.weatherCode
                                        -- NOTE: in theory this will never happen
                                        -- as we know the weather code it is "right now"
                                        |> Maybe.withDefault Api.ClearSky
                                    )
                                )
                            )
                        , el [ width fill, Font.center, Font.size 182, Font.medium ] (text (actualTemp ++ "°"))
                        ]

                _ ->
                    text "--"
            , dailySummary
            , statCards
            , -- Weekly Forecast
              el
                [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                , Font.heavy
                ]
                (text "Weekly Forecast")
            , el
                [ width fill
                ]
                (row
                    [ padding 15
                    , spacing 18
                    , width fill
                    , scrollbarX
                    ]
                    (List.map (\( date, code, max ) -> weeklyForecastCard date max code) apiData.daily)
                )

            -- Attribution
            , paragraph [ Font.alignRight, paddingEach { bottom = 0, top = 8, left = 0, right = 8 } ] [ text "Weather data by ", link [ Font.family [ Font.monospace ], Font.color (rgb 0 0 1) ] { label = text "Open-Meteo.com", url = "https://open-meteo.com/" } ]
            ]
        )


initialLoadingScreen : Element msg
initialLoadingScreen =
    column
        [ centerX, centerY, width fill ]
        [ paragraph [ Font.center, paddingXY 32 0 ]
            [ el [ Font.bold, Font.size 64 ] (text "LOADING")
            , br
            , br
            , el [ Font.semiBold, Font.size 18 ] (text "Did you know: ")
            , el [ Font.size 18 ] (text "Lightning strikes the Earth about 100 times per second.\n")
            ]
        ]


weeklyForecastCard : Posix -> Float -> WMOCode -> Element msg
weeklyForecastCard date max code =
    let
        month : String
        month =
            Time.toMonth Time.utc date |> monthToString

        day : String
        day =
            Time.toDay Time.utc date |> String.fromInt
    in
    column
        [ Border.color black
        , rounded 14
        , Border.solid
        , Border.width 3
        , padding 14
        , Font.bold
        , spacing 12
        ]
        [ el [ centerX ] (text (String.fromFloat max ++ "°"))
        , el [ centerX ] (wmoCodeToIcon code 24 Inherit |> Element.html)
        , el [ centerX, Font.size 14 ] (text (day ++ nbsp ++ String.left 3 month))
        ]


statCard : Color -> Icon msg -> String -> String -> Element msg
statCard primaryColor icon title value =
    column
        [ spacing 12
        , Font.color primaryColor
        ]
        [ el [ centerX, paddingBottom 8 ] (icon 52 Inherit |> Element.html)
        , el [ Font.regular, Font.size 24, centerX ] (text value)
        , el [ centerX, Font.size 14, Font.light ] (text title)
        ]



-- HELPERS


hourlyDataOfToday : Zone -> Posix -> List Hourly -> List Hourly
hourlyDataOfToday myZone currTime list =
    let
        year =
            Time.toYear myZone currTime

        month =
            Time.toMonth myZone currTime |> monthToInt

        day =
            Time.toDay myZone currTime
    in
    List.filter
        (\hourly ->
            let
                hourlyYear =
                    Time.toYear myZone hourly.time

                hourlyMonth =
                    Time.toMonth myZone hourly.time |> monthToInt

                hourlyDay =
                    Time.toDay myZone hourly.time
            in
            year == hourlyYear && month == hourlyMonth && day == hourlyDay
        )
        list


numberWithSign : Float -> String
numberWithSign n =
    if n < 0 then
        "-" ++ String.fromFloat n

    else if n > 0 then
        "+" ++ String.fromFloat n

    else
        String.fromFloat n


timeClosestToMine : Zone -> Posix -> Hourly -> List Hourly -> Hourly
timeClosestToMine zone time firstItem list =
    -- NOTE: could be made even more bulletproof and use a
    -- function that checks that it's a few hours before
    -- the user time, at most, and returns a Maybe Hourly instead
    let
        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time |> monthToInt

        day =
            Time.toDay zone time

        hour =
            Time.toHour zone time

        minute =
            Time.toMinute zone time
    in
    List.foldl
        (\a b ->
            let
                y =
                    Time.toYear Time.utc a.time

                m =
                    Time.toMonth Time.utc a.time |> monthToInt

                d =
                    Time.toDay Time.utc a.time

                h =
                    Time.toHour Time.utc a.time

                min =
                    Time.toMinute Time.utc a.time
            in
            if
                (y == year && m == month && d == day && h == hour && min == minute)
                    || (y == year && m == month && d == day && h == hour && min < minute)
                    || (y == year && m == month && d == day && h < hour)
                    || (y == year && m == month && d < day)
                    || (y == year && m < month)
                    || (y < year)
            then
                a

            else
                b
        )
        (List.head list |> Maybe.withDefault firstItem)
        list


toKm : Float -> Float
toKm meters =
    meters / 1000


nbsp : String
nbsp =
    "\u{00A0}"



{-
   Converted from https://github.com/eskimoblood/elm-color-extra/blob/5.1.0/src/Color/Convert.elm#L252
-}


toHex : Int -> String
toHex i =
    ParseInt.toRadix 16 i |> Result.withDefault "" >> String.padLeft 2 '0'



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


neFoldrMaybeListWithDefault : number -> Nonempty (Maybe number) -> (number -> number -> number) -> number
neFoldrMaybeListWithDefault num rest fn =
    NEList.foldl
        (\next acc ->
            case next of
                Just val ->
                    fn acc val

                Nothing ->
                    acc
        )
        num
        rest


foldrMaybeListWithDefault : number -> List (Maybe number) -> (number -> number -> number) -> number
foldrMaybeListWithDefault num rest fn =
    List.foldr
        (\next acc ->
            case next of
                Just val ->
                    fn acc val

                Nothing ->
                    acc
        )
        num
        rest
