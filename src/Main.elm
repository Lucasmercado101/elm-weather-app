module Main exposing (..)

import Animator
import Api exposing (Hourly, ResponseData, ReverseGeocodingResponse, WMOCode, wmoCodeToIcon, wmoCodeToString)
import Browser
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
    { apiData : ResponseData
    , currentRefetchingAnim : Animator.Timeline (RefetchingStatus Http.Error)
    , currentRefetchingStatus : RefetchingStatus Http.Error
    , location : ( Float, Float )
    , currentTime : Posix

    -- NOTE: depending on flags I may have the date
    -- before I actually have the zone to
    -- make the next request with the correct date
    , zone : Maybe Zone
    , primaryColor : Color
    , isOptionMenuOpen : Bool

    -- NOTE: could be fetched before on the loading screen
    -- but if the user is mainly on the MainScreen + caching then it's
    -- unnecessary overhead and logic that's not worth it
    -- NOTE: could be made into a Loading | Loaded | Error type union
    -- can't be bothered though
    , country : String
    , state : String
    , countryAndStateVisibility : Animator.Timeline Bool
    }


type LoadingScreenModel
    = LoadingScreenHasNoCurrentZone LoadingScreenModelNoZoneData
    | LoadingScreenHasCurrentZone LoadingScreenModelData


type alias LoadingScreenModelNoZoneData =
    { location : ( Float, Float )
    , currentTime : Posix
    }


type alias LoadingScreenModelData =
    { fetchingRequest : InitialWebRequest Http.Error
    , location : ( Float, Float )
    , currentTime : Posix
    , zone : Zone
    }


type Model
    = WelcomeScreen Welcome.WelcomeScreenModel
    | LoadingScreen LoadingScreenModel
    | MainScreen MainScreenModel



-- MESSAGE


type LoadingScreenMsg
    = RetryFetchingWeather
    | GotWeatherResponse (Result Http.Error ResponseData)
    | GotCurrentZoneLoadingScreenMsg Zone


type MainScreenMsg
    = ChangedPrimaryColor String
    | OpenOptionsMenu
    | CloseOptionsMenu
    | RefetchWeatherOnBackground
    | GotRefetchingWeatherResp (Result Http.Error ResponseData)
    | Tick Time.Posix
    | GotCurrentZoneMainScreen Zone
    | GotCurrentTimeMainScreen Posix
    | GotCountryAndStateMainScreen (Result Http.Error ReverseGeocodingResponse)


type Msg
    = OnWelcomeScreenMsg Welcome.WelcomeScreenMsg
    | OnLoadingScreenMsg LoadingScreenMsg
    | OnMainScreenMsg MainScreenMsg



-- MAIN
-- TODO: handle receiving error on
-- location: granted but error on attempting to get location


type Flags
    = TimeOnly { posixTimeNow : Int }
    | TimeAndLocationCoords { posixTimeNow : Int, latitude : Float, longitude : Float }
    | CachedWeatherData { posixTimeNow : Int, cachedWeatherData : Api.ResponseData }


posixFlagDecoder : JD.Decoder Flags
posixFlagDecoder =
    JD.field "posixTimeNow" JD.int
        |> JD.map (\l -> TimeOnly { posixTimeNow = l })


posixAndLocationsFlagDecoder : JD.Decoder Flags
posixAndLocationsFlagDecoder =
    JD.map3
        (\time latitude longitude ->
            TimeAndLocationCoords
                { posixTimeNow = time
                , latitude = latitude
                , longitude = longitude
                }
        )
        (JD.field "posixTimeNow" JD.int)
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
        Api.responseDataDecoder


flagsDecoders : JD.Value -> Result JD.Error Flags
flagsDecoders value =
    JD.decodeValue
        (JD.oneOf
            [ cachedWeatherDataFlagDecoder
            , posixAndLocationsFlagDecoder
            , posixFlagDecoder
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
                TimeOnly { posixTimeNow } ->
                    ( WelcomeScreen (Welcome.welcomeScreenInit posixTimeNow)
                    , Cmd.none
                    )

                -- NOTE: Very rare, realistically the data would've been
                -- already fetched as soon as coordinates were given
                TimeAndLocationCoords { posixTimeNow, latitude, longitude } ->
                    ( LoadingScreen
                        (LoadingScreenHasNoCurrentZone
                            { location = ( latitude, longitude )
                            , currentTime = Time.millisToPosix posixTimeNow
                            }
                        )
                    , Task.perform GotCurrentZoneLoadingScreenMsg Time.here |> Cmd.map OnLoadingScreenMsg
                    )

                CachedWeatherData { posixTimeNow, cachedWeatherData } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData
                    in
                    ( MainScreen
                        { apiData = cachedWeatherData
                        , currentRefetchingStatus = NotRefetching
                        , currentRefetchingAnim = Animator.init NotRefetching
                        , location = ( latitude, longitude )
                        , currentTime = Time.millisToPosix posixTimeNow
                        , zone = Nothing
                        , primaryColor = primary
                        , isOptionMenuOpen = False
                        , country = ""
                        , state = ""
                        , countryAndStateVisibility = Animator.init False
                        }
                    , Cmd.batch
                        [ Task.perform GotCurrentZoneMainScreen Time.here
                            |> Cmd.map OnMainScreenMsg
                        , Api.getReverseGeocoding ( latitude, longitude ) GotCountryAndStateMainScreen
                            |> Cmd.map OnMainScreenMsg
                        ]
                    )

        Err err ->
            -- NOTE: this will not happen unless i screw up the flags
            -- if i want to handle it: I will need to call Time.now in welcome to get the current time
            ( WelcomeScreen (Welcome.welcomeScreenInit 0)
            , Cmd.none
            )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update topMsg topModel =
    case ( topMsg, topModel ) of
        ( OnWelcomeScreenMsg msg, WelcomeScreen model ) ->
            Welcome.welcomeScreenUpdate msg model
                |> (\( a, b ) ->
                        case a.receivedLocation of
                            Just { latitude, longitude } ->
                                ( LoadingScreen
                                    (LoadingScreenHasNoCurrentZone
                                        { location = ( latitude, longitude )
                                        , currentTime = model.currentTime
                                        }
                                    )
                                , Task.perform GotCurrentZoneLoadingScreenMsg Time.here |> Cmd.map OnLoadingScreenMsg
                                )

                            Nothing ->
                                ( WelcomeScreen a, Cmd.map OnWelcomeScreenMsg b )
                   )

        ( OnLoadingScreenMsg msg, LoadingScreen model ) ->
            -- NOTE: could probably be refactored?
            -- doesn't seem to be worth it though
            case model of
                LoadingScreenHasNoCurrentZone modelData ->
                    case msg of
                        GotCurrentZoneLoadingScreenMsg zone ->
                            ( LoadingScreen
                                (LoadingScreenHasCurrentZone
                                    { zone = zone
                                    , location = modelData.location
                                    , currentTime = modelData.currentTime
                                    , fetchingRequest = Loading
                                    }
                                )
                            , Cmd.map OnLoadingScreenMsg (Api.getData modelData.location modelData.currentTime zone GotWeatherResponse)
                            )

                        -- NOTE: For future pattern matching error's sake
                        GotWeatherResponse _ ->
                            ( LoadingScreen (LoadingScreenHasNoCurrentZone modelData), Cmd.none )

                        RetryFetchingWeather ->
                            ( LoadingScreen (LoadingScreenHasNoCurrentZone modelData), Cmd.none )

                LoadingScreenHasCurrentZone modelData ->
                    case msg of
                        GotWeatherResponse result ->
                            case result of
                                Ok data ->
                                    ( MainScreen
                                        { apiData = data
                                        , currentRefetchingStatus = NotRefetching
                                        , currentRefetchingAnim = Animator.init NotRefetching
                                        , location = modelData.location
                                        , currentTime = modelData.currentTime
                                        , zone = Just modelData.zone
                                        , primaryColor = primary
                                        , isOptionMenuOpen = False
                                        , country = ""
                                        , state = ""
                                        , countryAndStateVisibility = Animator.init False
                                        }
                                    , let
                                        ( latitude, longitude ) =
                                            modelData.location
                                      in
                                      Api.getReverseGeocoding ( latitude, longitude ) GotCountryAndStateMainScreen
                                        |> Cmd.map OnMainScreenMsg
                                    )

                                Err err ->
                                    ( LoadingScreen
                                        (LoadingScreenHasCurrentZone
                                            { modelData
                                                | fetchingRequest = Failure err
                                            }
                                        )
                                    , Cmd.none
                                    )

                        RetryFetchingWeather ->
                            ( LoadingScreen (LoadingScreenHasCurrentZone { modelData | fetchingRequest = Loading })
                            , Cmd.map OnLoadingScreenMsg (Api.getData modelData.location modelData.currentTime modelData.zone GotWeatherResponse)
                            )

                        -- NOTE: will never make it here
                        GotCurrentZoneLoadingScreenMsg _ ->
                            ( LoadingScreen (LoadingScreenHasCurrentZone modelData), Cmd.none )

        ( OnMainScreenMsg msg, MainScreen model ) ->
            case msg of
                Tick newTime ->
                    ( model
                        |> Animator.update newTime animator
                    , Cmd.none
                    )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

                ChangedPrimaryColor hexColor ->
                    ( { model | primaryColor = hexColor |> hexToColor |> Result.withDefault model.primaryColor }, Cmd.none )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

                OpenOptionsMenu ->
                    ( { model | isOptionMenuOpen = True }, Cmd.none )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

                CloseOptionsMenu ->
                    ( { model | isOptionMenuOpen = False }, Cmd.none )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

                GotCountryAndStateMainScreen countryAndState ->
                    case countryAndState of
                        Ok { address } ->
                            ( MainScreen
                                { model
                                    | country = address.country
                                    , state = address.state
                                    , countryAndStateVisibility =
                                        model.countryAndStateVisibility
                                            |> Animator.go Animator.slowly True
                                }
                            , Cmd.none
                            )

                        Err _ ->
                            -- NOTE: not handling error on purpose
                            ( MainScreen
                                { model
                                    | country = ""
                                    , state = ""
                                    , countryAndStateVisibility =
                                        model.countryAndStateVisibility
                                            |> Animator.go Animator.slowly False
                                }
                            , Cmd.none
                            )

                RefetchWeatherOnBackground ->
                    (case model.zone of
                        Just zone ->
                            ( { model
                                | currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately Refetching
                                , currentRefetchingStatus = Refetching
                              }
                            , Cmd.batch
                                [ let
                                    ( latitude, longitude ) =
                                        model.location
                                  in
                                  Api.getReverseGeocoding ( latitude, longitude ) GotCountryAndStateMainScreen

                                --  TODO: currently refetching using given coordinates,
                                --  but it should be the coordinates given if no locations perms allowed
                                --  otherwise get current location and fetch using that, also add option to change
                                --  location on menu
                                , Api.getData model.location model.currentTime zone GotRefetchingWeatherResp
                                ]
                            )

                        Nothing ->
                            ( model, Cmd.none )
                    )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

                GotCurrentZoneMainScreen zone ->
                    -- NOTE: currently the states of 'hasZone' and 'hasNoZone' are "implicit"
                    -- i.e: i have to remember to change both and how they work together
                    -- will split it into two later
                    -- currently how it works is i don't fetch any data if i have no zone
                    -- and i fetch the zone if i have no zone, all this is
                    -- being set up in the flags, otherwise it works as normal
                    -- since LoadingScreen would pass zone to MainScreen
                    ( MainScreen
                        { model
                            | zone = Just zone
                            , currentRefetchingAnim =
                                model.currentRefetchingAnim
                                    |> Animator.go Animator.immediately Refetching
                            , currentRefetchingStatus = Refetching
                        }
                    , Api.getData model.location model.currentTime zone GotRefetchingWeatherResp
                        |> Cmd.map OnMainScreenMsg
                    )

                GotCurrentTimeMainScreen time ->
                    ( MainScreen
                        { model
                            | currentTime = time
                        }
                    , Cmd.none
                    )

                GotRefetchingWeatherResp result ->
                    (case result of
                        Ok data ->
                            ( { model
                                | apiData = data
                                , currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately NotRefetching
                                , currentRefetchingStatus = NotRefetching
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model
                                | currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately (Error err)
                                , currentRefetchingStatus = Error err
                              }
                            , Cmd.none
                            )
                    )
                        |> (\( a, b ) -> ( MainScreen a, Cmd.map OnMainScreenMsg b ))

        ( _, model ) ->
            ( model, Cmd.none )



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
loadingScreenView model =
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
            (case model of
                LoadingScreenHasNoCurrentZone _ ->
                    initialLoadingScreen

                LoadingScreenHasCurrentZone modelData ->
                    case modelData.fetchingRequest of
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
            [ row [ centerY, width fill, paddingBottom 15 ]
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
            , el
                [ centerX
                , Background.color black
                , paddingXY 16 8
                , Border.rounded 200
                ]
                (paragraph [ Font.color model.primaryColor, Font.size 14, Font.light ]
                    [ text
                        (model.currentTime
                            |> Time.toWeekday zone
                            |> dayToString
                        )
                    , text ", "
                    , text
                        (model.currentTime
                            |> Time.toDay zone
                            |> String.fromInt
                        )
                    , text " "
                    , text
                        (model.currentTime
                            |> Time.toMonth zone
                            |> monthToString
                        )
                    ]
                )
            , case model.apiData.hourly of
                x :: xs ->
                    let
                        closestHourly : Hourly
                        closestHourly =
                            timeClosestToMine zone model.currentTime x xs

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
            , -- Daily summary
              el
                [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                , Font.heavy
                ]
                (text "Daily summary")
            , case model.apiData.hourly of
                x :: xs ->
                    let
                        closestHourly : Hourly
                        closestHourly =
                            timeClosestToMine zone model.currentTime x xs

                        actualTemp : String
                        actualTemp =
                            case closestHourly.temperature of
                                Just val ->
                                    val |> numberWithSign

                                Nothing ->
                                    -- NOTE: in theory this will never happen
                                    -- as we know what temperature it is "right now"
                                    "--"

                        perceivedTemp : String
                        perceivedTemp =
                            case closestHourly.apparentTemperature of
                                Just val ->
                                    val |> numberWithSign

                                Nothing ->
                                    -- NOTE: in theory this will never happen
                                    -- as we know what temperature it is "right now"
                                    "--"

                        todayHourlyData : List Hourly
                        todayHourlyData =
                            hourlyDataOfToday zone model.currentTime model.apiData.hourly

                        lowestTempOfToday : String
                        lowestTempOfToday =
                            todayHourlyData
                                |> List.map .temperature
                                |> List.foldr
                                    (\l ->
                                        case l of
                                            Just val ->
                                                Maybe.map (min val)

                                            Nothing ->
                                                Maybe.map (min 0)
                                    )
                                    (Just 999)
                                |> Maybe.withDefault 0
                                |> numberWithSign

                        highestTempOfToday : String
                        highestTempOfToday =
                            todayHourlyData
                                |> List.map .temperature
                                |> List.foldr
                                    (\l ->
                                        case l of
                                            Just val ->
                                                Maybe.map (max val)

                                            Nothing ->
                                                Maybe.map (max 0)
                                    )
                                    (Just 0)
                                |> Maybe.withDefault 0
                                |> numberWithSign
                    in
                    paragraph
                        [ paddingEach { top = 14, left = 15, right = 0, bottom = 0 }
                        , Font.bold
                        , Font.size 16
                        , width fill
                        ]
                        [ text ("Now it feels like " ++ perceivedTemp ++ "°, it's actually " ++ actualTemp ++ "°")
                        , br
                        , text ("Today, the temperature is felt in the range from " ++ lowestTempOfToday ++ "° to " ++ highestTempOfToday ++ "°")
                        ]

                _ ->
                    none
            , column
                [ width fill
                ]
                [ el
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
                        [ statCard model.primaryColor
                            Icons.air
                            (case model.apiData.hourly of
                                x :: xs ->
                                    (timeClosestToMine zone model.currentTime x xs
                                        |> .windSpeed
                                        -- NOTE: in theory this will never happen
                                        -- as we know what temperature it is "right now"
                                        |> Maybe.withDefault 0
                                        |> round
                                        |> String.fromInt
                                    )
                                        ++ "km/h"

                                _ ->
                                    -- NOTE: in theory it will never reach here
                                    -- as there will always be one item in the list
                                    -- either way it's handled as "--" in all 3 stat cards
                                    "--"
                            )
                            "Wind"
                        , statCard model.primaryColor
                            Outlined.water_drop
                            (case model.apiData.hourly of
                                x :: xs ->
                                    (timeClosestToMine zone model.currentTime x xs
                                        |> .relativeHumidity
                                        |> String.fromInt
                                    )
                                        ++ "%"

                                _ ->
                                    "--"
                            )
                            "Humidity"
                        , statCard model.primaryColor
                            Outlined.visibility
                            (case model.apiData.hourly of
                                x :: xs ->
                                    (timeClosestToMine zone model.currentTime x xs
                                        |> .visibility
                                        |> toKm
                                        |> round
                                        |> String.fromInt
                                    )
                                        ++ "km"

                                _ ->
                                    "--"
                            )
                            "Visibility"
                        ]
                    )
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
                        (List.map (\( date, code, max ) -> weeklyForecastCard date max code) model.apiData.daily)
                    )

                -- Attribution
                , paragraph [ Font.alignRight, paddingEach { bottom = 0, top = 8, left = 0, right = 8 } ] [ text "Weather data by ", link [ Font.family [ Font.monospace ], Font.color (rgb 0 0 1) ] { label = text "Open-Meteo.com", url = "https://open-meteo.com/" } ]
                ]
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
statCard primaryColor icon value title =
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
                    Time.toYear Time.utc hourly.time

                hourlyMonth =
                    Time.toMonth Time.utc hourly.time |> monthToInt

                hourlyDay =
                    Time.toDay Time.utc hourly.time
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


timeClosestToMine : Zone -> Posix -> { a | time : Posix } -> List { a | time : Posix } -> { a | time : Posix }
timeClosestToMine zone time firstItem list =
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
