module Main exposing (..)

import Animator
import Api exposing (Hourly, ResponseData, ReverseGeocodingResponse, WMOCode, esWmoCodeToString, wmoCodeToIcon, wmoCodeToString)
import Browser
import Cmd.Extra exposing (pure)
import Element exposing (Color, Element, alpha, centerX, centerY, column, el, fill, height, inFront, layout, link, maximum, minimum, none, padding, paddingEach, paddingXY, paragraph, px, rgb, rotate, row, scrollbarX, scrollbarY, spaceEvenly, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input exposing (button)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as JD
import List.Nonempty as NEList exposing (Nonempty(..))
import Localizations exposing (Language(..))
import MIcons exposing (..)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import ParseInt
import Ports
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
            Sub.batch
                [ Ports.locationReceiver ReceivedGeoLocation
                , Ports.errorObtainingCurrentPosition RequestLocationPermsApiError
                , Ports.noGeoLocationApiAvailableReceiver NoGeoLocationApi
                , animator |> Animator.toSubscription Tick modelData
                ]
                |> Sub.map OnMainScreenMsg

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


type Location
    = UsingGeoLocation Coordinates
    | FixedCoordinates Coordinates


type alias EnteringManualCoordinates =
    { latitude : String
    , longitude : String
    , error : Maybe Localizations.LatAndLongManualError
    }


type OptionMenu
    = Closed
    | Open (Maybe EnteringManualCoordinates)


type alias MainScreenModel =
    { currentRefetchingAnim : Animator.Timeline (RefetchingStatus Http.Error)
    , currentRefetchingStatus : RefetchingStatus Http.Error
    , location : Location
    , primaryColor : Color
    , secondaryColor : Color
    , optionMenu : OptionMenu
    , zone : Maybe Zone
    , language : Language

    -- TODO: do better
    , isOnThemePage : Bool

    -- NOTE: when I fetch I return response and current time posix
    -- they're synced as I don't need to use posix anywhere else
    -- but when I get the data and to do things at the time I fetched it
    , apiData : ( ResponseData, Posix )

    -- NOTE: could be made into a Loading | Loaded | Error type union
    -- can't be bothered though
    , currentAddress : Maybe Api.Address
    , countryAndStateVisibility : Animator.Timeline Bool
    }


type alias LoadingScreenModel =
    { fetchingStatus : InitialWebRequest Http.Error
    , coordinates : Coordinates
    , isUsingGeoLocation : Bool
    , language : Language
    }


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
    | RefetchDataOnBackground
    | GotRefetchingWeatherResp (Result Http.Error ( ResponseData, Posix, Zone ))
    | Tick Time.Posix
    | GotCountryAndStateMainScreen (Result Http.Error ReverseGeocodingResponse)
    | ReceivedGeoLocation { latitude : Float, longitude : Float }
      -- Options menu
    | OpenOptionsMenu
    | CloseOptionsMenu
    | ToggleGeoLocation
    | RequestLocationPermsApiError Int
    | NoGeoLocationApi ()
    | ShowManualCoordinatesForm
    | OnChangeLatitude String
    | OnChangeLongitude String
    | SubmitManualLocationForm
    | CancelManualForm
    | ToggleLanguage
      -- Theme selection screen
    | GoToThemeSelectionPage
    | CloseThemeSelectorScreen


type Msg
    = OnWelcomeScreenMsg Welcome.WelcomeScreenMsg
    | OnLoadingScreenMsg LoadingScreenMsg
    | OnMainScreenMsg MainScreenMsg



-- MAIN
-- TODO: handle receiving error on
-- location: granted but error on attempting to get location


type Flags
    = LanguageOnly
        { language : String
        }
    | CachedWeatherData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , usingGeoLocation : Bool
        , language : String
        }
    | CachedWeatherAndAddressData
        { posixTimeNow : Int
        , cachedWeatherData : Api.ResponseData
        , country : String
        , state : Maybe String
        , city : Maybe String
        , usingGeoLocation : Bool
        , language : String
        }


languageFlagDecoder : JD.Decoder Flags
languageFlagDecoder =
    JD.map
        (\language ->
            LanguageOnly
                { language = language
                }
        )
        (JD.field "language" JD.string)


cachedWeatherDataFlagDecoder : JD.Decoder Flags
cachedWeatherDataFlagDecoder =
    JD.map4
        (\time weatherData usingGeo language ->
            CachedWeatherData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , usingGeoLocation = usingGeo
                , language = language
                }
        )
        (JD.field "posixTimeNow" JD.int)
        (JD.field "cachedWeatherData" Api.responseDataDecoder)
        (JD.field "usingGeoLocation" JD.bool)
        (JD.field "language" JD.string)


cachedWeatherAndAddressDataDecoder : JD.Decoder Flags
cachedWeatherAndAddressDataDecoder =
    JD.map7
        (\time weatherData country maybeState maybeCity usingGeo language ->
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
            CachedWeatherAndAddressData
                { posixTimeNow = time
                , cachedWeatherData = weatherData
                , country = country
                , state = state
                , usingGeoLocation = usingGeo
                , city = city
                , language = language
                }
        )
        (JD.field "posixTimeNow" JD.int)
        (JD.field "cachedWeatherData" Api.responseDataDecoder)
        (JD.field "country" JD.string)
        (JD.maybe (JD.field "state" JD.string))
        (JD.maybe (JD.field "city" JD.string))
        (JD.field "usingGeoLocation" JD.bool)
        (JD.field "language" JD.string)


flagsDecoders : JD.Value -> Result JD.Error Flags
flagsDecoders value =
    JD.decodeValue
        (JD.oneOf
            [ cachedWeatherAndAddressDataDecoder
            , cachedWeatherDataFlagDecoder
            , languageFlagDecoder
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


init : JD.Value -> ( Model, Cmd Msg )
init val =
    let
        langParse langStr =
            if langStr == "es" || String.contains "es-" langStr then
                Spanish

            else
                English
    in
    case flagsDecoders val of
        Ok flags ->
            case flags of
                CachedWeatherAndAddressData { cachedWeatherData, posixTimeNow, country, state, city, usingGeoLocation, language } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData
                    in
                    ( { apiData = ( cachedWeatherData, Time.millisToPosix posixTimeNow )
                      , currentRefetchingStatus = Refetching
                      , currentRefetchingAnim = Animator.init Refetching
                      , language = langParse language
                      , isOnThemePage = False
                      , location =
                            if usingGeoLocation == True then
                                UsingGeoLocation { latitude = latitude, longitude = longitude }

                            else
                                FixedCoordinates { latitude = latitude, longitude = longitude }

                      -- TODO: get from cache
                      , primaryColor = defaultPrimary
                      , secondaryColor = defaultSecondary
                      , optionMenu = Closed
                      , currentAddress = Just { city = city, state = state, country = country }
                      , countryAndStateVisibility = Animator.init True

                      -- TODO: handle zone, when refreshing there's no good initial value
                      -- TODO: send zone when I have it and cache it
                      -- and get it from init https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/TimeZone#zones
                      , zone = Just Time.utc
                      }
                    , if usingGeoLocation == True then
                        Ports.requestLoc

                      else
                        Cmd.batch
                            [ Api.getReverseGeocoding { latitude = latitude, longitude = longitude } GotCountryAndStateMainScreen
                            , Api.getWeatherData { latitude = latitude, longitude = longitude } |> Task.attempt GotRefetchingWeatherResp
                            ]
                    )
                        |> mapToMainScreen

                CachedWeatherData { cachedWeatherData, posixTimeNow, usingGeoLocation, language } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData
                    in
                    ( { apiData = ( cachedWeatherData, Time.millisToPosix posixTimeNow )
                      , currentRefetchingStatus = Refetching
                      , currentRefetchingAnim = Animator.init Refetching
                      , language = langParse language
                      , isOnThemePage = False
                      , location =
                            if usingGeoLocation == True then
                                UsingGeoLocation { latitude = latitude, longitude = longitude }

                            else
                                FixedCoordinates { latitude = latitude, longitude = longitude }

                      -- TODO: get from cache
                      , primaryColor = defaultPrimary
                      , secondaryColor = defaultSecondary
                      , optionMenu = Closed
                      , currentAddress = Nothing
                      , countryAndStateVisibility = Animator.init False

                      -- TODO: handle zone, when refreshing there's no good initial value
                      -- TODO: send zone when I have it and cache it
                      -- and get it from init https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/TimeZone#zones
                      , zone = Just Time.utc
                      }
                    , if usingGeoLocation == True then
                        Ports.requestLoc

                      else
                        Cmd.batch
                            [ Api.getReverseGeocoding { latitude = latitude, longitude = longitude } GotCountryAndStateMainScreen
                            , Api.getWeatherData { latitude = latitude, longitude = longitude } |> Task.attempt GotRefetchingWeatherResp
                            ]
                    )
                        |> mapToMainScreen

                LanguageOnly { language } ->
                    WelcomeScreen (Welcome.welcomeScreenInit (langParse language)) |> pure

        Err _ ->
            -- NOTE: this will not happen unless i screw up the flags
            WelcomeScreen (Welcome.welcomeScreenInit English)
                |> pure



-- UPDATE


mapToMainScreen : ( MainScreenModel, Cmd MainScreenMsg ) -> ( Model, Cmd Msg )
mapToMainScreen ( a, b ) =
    ( MainScreen a, Cmd.map OnMainScreenMsg b )


update : Msg -> Model -> ( Model, Cmd Msg )
update topMsg topModel =
    case ( topMsg, topModel ) of
        ( OnWelcomeScreenMsg ms, WelcomeScreen md ) ->
            Welcome.welcomeScreenUpdate ms md
                |> (\( welcomeScreenModel, welcomeScreenMessage ) ->
                        case welcomeScreenModel.receivedLocation of
                            Just coords ->
                                ( LoadingScreen
                                    { fetchingStatus = Loading
                                    , coordinates = coords
                                    , isUsingGeoLocation = welcomeScreenModel.usingGeoLocation
                                    , language = welcomeScreenModel.language
                                    }
                                , Api.getWeatherData coords
                                    |> Task.attempt (\l -> OnLoadingScreenMsg (GotWeatherResponse l))
                                  -- NOTE: Api.getReverseGeocoding could be called here
                                  -- to pass it along to MainScreen but it's not really worth it
                                )

                            Nothing ->
                                ( WelcomeScreen welcomeScreenModel, Cmd.map OnWelcomeScreenMsg welcomeScreenMessage )
                   )

        ( OnLoadingScreenMsg msg, LoadingScreen model ) ->
            -- NOTE: could probably be refactored into 2 states in MSG
            -- instead of one 2 states variants in 1 MSG variant
            case msg of
                GotWeatherResponse result ->
                    case result of
                        Ok ( data, currentTime, zone ) ->
                            ( MainScreen
                                { apiData = ( data, currentTime )
                                , currentRefetchingStatus = NotRefetching
                                , currentRefetchingAnim = Animator.init NotRefetching
                                , language = model.language
                                , location =
                                    if model.isUsingGeoLocation then
                                        UsingGeoLocation model.coordinates

                                    else
                                        FixedCoordinates model.coordinates
                                , isOnThemePage = False
                                , zone = Just zone

                                -- TODO: get from cache
                                , primaryColor = defaultPrimary
                                , secondaryColor = defaultSecondary
                                , optionMenu = Closed
                                , currentAddress = Nothing
                                , countryAndStateVisibility = Animator.init False
                                }
                            , Api.getReverseGeocoding { latitude = data.latitude, longitude = data.longitude } GotCountryAndStateMainScreen
                                |> Cmd.map OnMainScreenMsg
                            )

                        Err err ->
                            LoadingScreen { model | fetchingStatus = Failure err } |> pure

                RetryFetchingWeather ->
                    ( LoadingScreen { model | fetchingStatus = Loading }
                    , Api.getWeatherData model.coordinates
                        |> Task.attempt (\l -> OnLoadingScreenMsg (GotWeatherResponse l))
                    )

        ( OnMainScreenMsg msg, MainScreen model ) ->
            case msg of
                Tick newTime ->
                    (model |> Animator.update newTime animator)
                        |> pure
                        |> mapToMainScreen

                -- Theme screen
                CloseThemeSelectorScreen ->
                    { model | isOnThemePage = False }
                        |> pure
                        |> mapToMainScreen

                -- Options menu
                GoToThemeSelectionPage ->
                    { model | isOnThemePage = True, optionMenu = Closed }
                        |> pure
                        |> mapToMainScreen

                ToggleLanguage ->
                    { model
                        | language =
                            case model.language of
                                English ->
                                    Spanish

                                Spanish ->
                                    English
                    }
                        |> pure
                        |> mapToMainScreen

                ChangedPrimaryColor hexColor ->
                    { model | primaryColor = hexColor |> hexToColor |> Result.withDefault model.primaryColor }
                        |> pure
                        |> mapToMainScreen

                OpenOptionsMenu ->
                    { model | optionMenu = Open Nothing }
                        |> pure
                        |> mapToMainScreen

                CloseOptionsMenu ->
                    { model | optionMenu = Closed }
                        |> pure
                        |> mapToMainScreen

                ShowManualCoordinatesForm ->
                    case model.location of
                        UsingGeoLocation coords ->
                            { model
                                | optionMenu =
                                    Open
                                        (Just
                                            { latitude = coords.latitude |> String.fromFloat
                                            , longitude = coords.longitude |> String.fromFloat
                                            , error = Nothing
                                            }
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        FixedCoordinates coords ->
                            { model
                                | optionMenu =
                                    Open
                                        (Just
                                            { latitude = coords.latitude |> String.fromFloat
                                            , longitude = coords.longitude |> String.fromFloat
                                            , error = Nothing
                                            }
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                OnChangeLatitude newLatitude ->
                    case model.optionMenu of
                        Open (Just isEditingCoordinatesManually) ->
                            { model
                                | optionMenu =
                                    Open
                                        (Just
                                            { isEditingCoordinatesManually
                                                | latitude = newLatitude
                                            }
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                OnChangeLongitude newLongitude ->
                    case model.optionMenu of
                        Open (Just isEditingCoordinatesManually) ->
                            { model
                                | optionMenu =
                                    Open
                                        (Just
                                            { isEditingCoordinatesManually
                                                | longitude = newLongitude
                                            }
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                CancelManualForm ->
                    case model.optionMenu of
                        Open (Just _) ->
                            { model | optionMenu = Open Nothing }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                SubmitManualLocationForm ->
                    case model.optionMenu of
                        Open (Just manualCoordinates) ->
                            let
                                { latitude, longitude } =
                                    manualCoordinates

                                setManualLocationError error =
                                    { model
                                        | optionMenu =
                                            Open
                                                (Just
                                                    { manualCoordinates
                                                        | error = error
                                                    }
                                                )
                                    }
                                        |> pure
                                        |> mapToMainScreen
                            in
                            case String.toFloat latitude of
                                Just latFloat ->
                                    if latFloat < -90 || latFloat > 90 then
                                        setManualLocationError (Just Localizations.OutOfRangeLatitude)

                                    else
                                        case String.toFloat longitude of
                                            Just lonFloat ->
                                                if lonFloat < -180 || lonFloat > 180 then
                                                    setManualLocationError (Just Localizations.OutOfRangeLongitude)

                                                else
                                                    ( { model
                                                        | location = FixedCoordinates { latitude = latFloat, longitude = lonFloat }
                                                        , currentRefetchingStatus = Refetching
                                                        , currentRefetchingAnim = Animator.init Refetching
                                                        , optionMenu = Open Nothing
                                                      }
                                                    , Cmd.batch
                                                        [ Api.getReverseGeocoding { latitude = latFloat, longitude = lonFloat } GotCountryAndStateMainScreen
                                                        , Api.getWeatherData { latitude = latFloat, longitude = lonFloat } |> Task.attempt GotRefetchingWeatherResp
                                                        ]
                                                    )
                                                        |> mapToMainScreen

                                            Nothing ->
                                                setManualLocationError (Just Localizations.InvalidLongitude)

                                Nothing ->
                                    setManualLocationError (Just Localizations.InvalidLatitude)

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                -- NOTE: only changing if:
                -- location allowed and no geo api errors
                ToggleGeoLocation ->
                    case model.location of
                        UsingGeoLocation fixedCoordinates ->
                            { model | location = FixedCoordinates fixedCoordinates }
                                |> pure
                                |> mapToMainScreen

                        FixedCoordinates _ ->
                            -- NOTE: not doing something with the error on purpose
                            ( model, Ports.requestLoc )
                                |> mapToMainScreen

                RequestLocationPermsApiError _ ->
                    model
                        |> pure
                        |> mapToMainScreen

                NoGeoLocationApi _ ->
                    -- NOTE: prevent the user from changing the
                    -- Geolocation perms in the future as they
                    -- don't have a Geolocation api?
                    model
                        |> pure
                        |> mapToMainScreen

                -- Background refetching
                GotCountryAndStateMainScreen countryAndState ->
                    case countryAndState of
                        Ok { address } ->
                            { model
                                | currentAddress = Just address
                                , countryAndStateVisibility =
                                    model.countryAndStateVisibility
                                        |> Animator.go Animator.slowly True
                            }
                                |> pure
                                |> mapToMainScreen

                        Err _ ->
                            -- NOTE: not doing something with the error on purpose
                            { model
                                | currentAddress = Nothing
                                , countryAndStateVisibility =
                                    model.countryAndStateVisibility
                                        |> Animator.go Animator.slowly False
                            }
                                |> pure
                                |> mapToMainScreen

                RefetchDataOnBackground ->
                    ( { model
                        | currentRefetchingAnim =
                            model.currentRefetchingAnim
                                |> Animator.go Animator.immediately Refetching
                        , currentRefetchingStatus = Refetching
                      }
                    , case model.location of
                        UsingGeoLocation _ ->
                            Ports.requestLoc

                        FixedCoordinates coords ->
                            Cmd.batch
                                [ Api.getReverseGeocoding coords GotCountryAndStateMainScreen
                                , Api.getWeatherData coords |> Task.attempt GotRefetchingWeatherResp
                                ]
                    )
                        |> mapToMainScreen

                ReceivedGeoLocation coords ->
                    case model.optionMenu of
                        Open (Just _) ->
                            ( { model | optionMenu = Open Nothing, location = UsingGeoLocation coords }
                            , Cmd.batch
                                [ Api.getReverseGeocoding coords GotCountryAndStateMainScreen
                                , Api.getWeatherData coords |> Task.attempt GotRefetchingWeatherResp
                                ]
                            )
                                |> mapToMainScreen

                        _ ->
                            ( { model | location = UsingGeoLocation coords }
                            , Cmd.batch
                                [ Api.getReverseGeocoding coords GotCountryAndStateMainScreen
                                , Api.getWeatherData coords |> Task.attempt GotRefetchingWeatherResp
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
                    (let
                        divider =
                            el [ width fill, height (px 1), Background.color modelData.primaryColor ] none
                     in
                     case modelData.optionMenu of
                        Open isEnteringManualCoordinates ->
                            column
                                [ width fill
                                , Background.color modelData.secondaryColor
                                ]
                                [ button [ width fill ]
                                    { label =
                                        row
                                            [ width fill
                                            , height (px 52)
                                            , paddingX 15
                                            ]
                                            [ el
                                                [ width fill
                                                , Font.color modelData.primaryColor
                                                , Font.heavy
                                                ]
                                                (text (Localizations.theme modelData.language))
                                            , el
                                                [ Font.color modelData.primaryColor ]
                                                (Icons.chevron_right 40 Inherit |> Element.html)
                                            ]
                                    , onPress = Just GoToThemeSelectionPage
                                    }
                                , divider

                                -- Geo location
                                , row
                                    [ width fill
                                    , height (px 52)
                                    , paddingX 15
                                    ]
                                    [ el
                                        [ width fill
                                        , Font.color modelData.primaryColor
                                        , Font.heavy
                                        ]
                                        (text (Localizations.geolocation modelData.language))
                                    , button
                                        []
                                        (case modelData.location of
                                            UsingGeoLocation _ ->
                                                { label =
                                                    row
                                                        [ Border.color modelData.primaryColor
                                                        , Border.width 3
                                                        ]
                                                        [ el
                                                            [ Font.color modelData.primaryColor
                                                            , paddingXY 8 5
                                                            , Font.heavy
                                                            ]
                                                            (text "ON")
                                                        , el
                                                            [ Background.color modelData.primaryColor
                                                            , Font.heavy
                                                            , paddingXY 8 5
                                                            ]
                                                            (text "OFF")
                                                        ]
                                                , onPress = Just ToggleGeoLocation
                                                }

                                            FixedCoordinates _ ->
                                                { label =
                                                    row
                                                        [ Border.color modelData.primaryColor
                                                        , Border.width 3
                                                        ]
                                                        [ el
                                                            [ Background.color modelData.primaryColor
                                                            , Font.heavy
                                                            , paddingXY 8 5
                                                            ]
                                                            (text "ON")
                                                        , el
                                                            [ Font.color modelData.primaryColor
                                                            , paddingXY 8 5
                                                            , Font.heavy
                                                            , centerX
                                                            ]
                                                            (text "OFF")
                                                        ]
                                                , onPress = Just ToggleGeoLocation
                                                }
                                        )
                                    ]
                                , row
                                    [ width fill
                                    , height (px 52)
                                    , paddingX 15
                                    ]
                                    [ el
                                        [ width fill
                                        , Font.color modelData.primaryColor
                                        , Font.heavy
                                        ]
                                        (text (Localizations.coordinates modelData.language))
                                    , case modelData.location of
                                        FixedCoordinates coordinates ->
                                            button
                                                [ Background.color modelData.primaryColor
                                                , Font.heavy
                                                , paddingXY 5 5
                                                ]
                                                { label = text (format usLocale coordinates.latitude ++ ", " ++ format usLocale coordinates.longitude)
                                                , onPress =
                                                    case modelData.optionMenu of
                                                        Open (Just _) ->
                                                            Nothing

                                                        _ ->
                                                            Just ShowManualCoordinatesForm
                                                }

                                        UsingGeoLocation coordinates ->
                                            el [ Font.color modelData.primaryColor, Font.heavy ] (text (format usLocale coordinates.latitude ++ ", " ++ format usLocale coordinates.longitude))
                                    ]
                                , case isEnteringManualCoordinates of
                                    Just manualCoordinates ->
                                        column
                                            [ centerX
                                            , Background.color modelData.secondaryColor
                                            , width fill
                                            ]
                                            [ --  Error message
                                              case manualCoordinates.error of
                                                Just err ->
                                                    column [ width fill ]
                                                        [ paragraph
                                                            [ paddingXY 24 12
                                                            , Font.center
                                                            , spacing 8
                                                            , Font.color modelData.primaryColor
                                                            ]
                                                            [ el
                                                                [ centerX
                                                                , Font.heavy
                                                                , Font.underline
                                                                , Font.size 22
                                                                ]
                                                                (text "Error")
                                                            , br
                                                            , el
                                                                [ centerX
                                                                , Font.light
                                                                , Font.size 18
                                                                ]
                                                                (text (Localizations.manualLatitudeAndLongitudeError modelData.language err))
                                                            ]
                                                        , el [ width fill, height (px 1), Background.color modelData.primaryColor ] none
                                                        ]

                                                Nothing ->
                                                    none

                                            --   Latitude and Longitude form
                                            , row
                                                [ paddingEach
                                                    { top = 8
                                                    , bottom = 15
                                                    , left = 15
                                                    , right = 15
                                                    }
                                                , width fill
                                                , spacing 24
                                                ]
                                                [ Input.text
                                                    [ width fill
                                                    , Background.color modelData.primaryColor
                                                    ]
                                                    { onChange = OnChangeLatitude
                                                    , text = manualCoordinates.latitude
                                                    , placeholder = Just (Input.placeholder [] (el [ Font.color modelData.secondaryColor, alpha 0.65 ] (text manualCoordinates.latitude)))
                                                    , label = Input.labelAbove [ Font.color modelData.primaryColor ] (text (Localizations.latitude modelData.language ++ ":"))
                                                    }
                                                , Input.text
                                                    [ width fill
                                                    , Background.color modelData.primaryColor
                                                    ]
                                                    { onChange = OnChangeLongitude
                                                    , text = manualCoordinates.longitude
                                                    , placeholder = Just (Input.placeholder [] (el [ Font.color modelData.secondaryColor, alpha 0.65 ] (text manualCoordinates.longitude)))
                                                    , label = Input.labelAbove [ Font.color modelData.primaryColor ] (text (Localizations.longitude modelData.language ++ ":"))
                                                    }
                                                ]
                                            , row
                                                [ width fill
                                                , Font.color modelData.primaryColor
                                                ]
                                                [ button
                                                    [ paddingXY 24 12
                                                    , Font.center
                                                    , Font.bold
                                                    , Font.size 22
                                                    , width fill
                                                    ]
                                                    { label = text (Localizations.cancel modelData.language), onPress = Just CancelManualForm }
                                                , el [ width (px 1), height fill, Background.color modelData.primaryColor ] none
                                                , button
                                                    [ paddingXY 24 12
                                                    , Font.center
                                                    , Font.bold
                                                    , Font.size 22
                                                    , width fill
                                                    ]
                                                    { label = text (Localizations.confirm modelData.language), onPress = Just SubmitManualLocationForm }
                                                ]
                                            ]

                                    Nothing ->
                                        none
                                , divider
                                , row
                                    [ width fill
                                    , height (px 52)
                                    , paddingX 15
                                    ]
                                    [ el
                                        [ width fill
                                        , Font.color modelData.primaryColor
                                        , Font.heavy
                                        ]
                                        (text (Localizations.languagePicker modelData.language))
                                    , button
                                        []
                                        (case modelData.language of
                                            English ->
                                                { label =
                                                    row
                                                        [ Border.color modelData.primaryColor
                                                        , Border.width 3
                                                        ]
                                                        [ el
                                                            [ Font.color modelData.primaryColor
                                                            , paddingXY 8 5
                                                            , Font.heavy
                                                            ]
                                                            (text "EN")
                                                        , el
                                                            [ Background.color modelData.primaryColor
                                                            , Font.heavy
                                                            , paddingXY 8 5
                                                            ]
                                                            (text "ES")
                                                        ]
                                                , onPress = Just ToggleLanguage
                                                }

                                            Spanish ->
                                                { label =
                                                    row
                                                        [ Border.color modelData.primaryColor
                                                        , Border.width 3
                                                        ]
                                                        [ el
                                                            [ Background.color modelData.primaryColor
                                                            , Font.heavy
                                                            , paddingXY 8 5
                                                            ]
                                                            (text "EN")
                                                        , el
                                                            [ Font.color modelData.primaryColor
                                                            , paddingXY 8 5
                                                            , Font.heavy
                                                            , centerX
                                                            ]
                                                            (text "ES")
                                                        ]
                                                , onPress = Just ToggleLanguage
                                                }
                                        )
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
                                        { label = text "X", onPress = Just CloseOptionsMenu }
                                    ]
                                , divider
                                ]

                        Closed ->
                            none
                    )
                        |> Element.map OnMainScreenMsg

                LoadingScreen _ ->
                    none

                WelcomeScreen noLocationDataModel ->
                    if noLocationDataModel.geoLocationApiError /= "" then
                        paragraph
                            [ Background.color defaultSecondary
                            , Font.color defaultPrimary
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
                if m.isOnThemePage then
                    themeSelectorScreen m.language m.primaryColor m.secondaryColor

                else
                    mainScreen m |> Element.map OnMainScreenMsg
        )


loadingScreenView : LoadingScreenModel -> Element Msg
loadingScreenView { fetchingStatus } =
    el
        [ width fill
        , height fill
        ]
        (el
            [ width fill
            , height fill
            , Background.color defaultPrimary
            , paddingEach { top = 15, bottom = 16, left = 0, right = 0 }
            ]
            (case fetchingStatus of
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
                                , Background.color defaultSecondary
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



-- NOTE: currently showing data for the most recent hour
-- i.e: Current temperature, wind, humidity, visibility, etc.


mainScreen : MainScreenModel -> Element MainScreenMsg
mainScreen model =
    let
        hasHourlyDataOfToday =
            apiData.hourly |> hourlyDataOfToday zone currentTime |> NEList.fromList

        -- NOTE: this is just accounting for the brief moment
        -- if and when I don't have the user's time zone
        -- TODO: could be made better by cache through flags
        zone =
            Maybe.withDefault Time.utc model.zone

        ( apiData, currentTime ) =
            model.apiData

        currentDateChip : Element MainScreenMsg
        currentDateChip =
            el
                [ centerX
                , Background.color model.secondaryColor
                , paddingXY 16 8
                , Border.rounded 200
                ]
                (let
                    ( weekDay, month ) =
                        Localizations.dayAndMonth model.language
                            ( currentTime
                                |> Time.toWeekday zone
                            , currentTime
                                |> Time.toMonth zone
                            )

                    day =
                        currentTime
                            |> Time.toDay zone
                            |> String.fromInt
                 in
                 paragraph [ Font.color model.primaryColor, Font.size 14, Font.light ]
                    [ text weekDay
                    , text ", "
                    , text day
                    , text " "
                    , text month
                    ]
                )

        dailySummary : Element MainScreenMsg
        dailySummary =
            -- NOTE: could be even more strict and only a custom list type with 24
            -- list items, i.e the hours in a day, each being an Hourly,
            -- definitely overkill.
            case hasHourlyDataOfToday of
                Just ((Nonempty firstHourly restHourly) as todayHourlyData) ->
                    column []
                        [ el
                            [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                            , Font.heavy
                            ]
                            (text (Localizations.dailySummary model.language))
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
                            [ paddingEach { top = 14, left = 15, right = 19, bottom = 0 }
                            , Font.bold
                            , Font.size 16
                            , width fill
                            ]
                            [ Localizations.nowItFeels model.language
                                closestHourly.apparentTemperature
                                closestHourly.temperature
                            , br
                            , Localizations.temperatureRange model.language
                                lowestTempOfToday
                                highestTempOfToday
                            ]
                        ]

                _ ->
                    none

        bigCurrentTemperature : Element MainScreenMsg
        bigCurrentTemperature =
            case hasHourlyDataOfToday of
                Just (Nonempty firstHourly restHourly) ->
                    let
                        closestHourly : Hourly
                        closestHourly =
                            timeClosestToMine zone currentTime firstHourly restHourly

                        actualTemp : String
                        actualTemp =
                            case closestHourly.temperature of
                                Just val ->
                                    val |> round |> String.fromInt

                                Nothing ->
                                    -- NOTE: it can come as null in the JSON
                                    "--"
                    in
                    column [ width fill ]
                        [ el [ width fill, Font.center, Font.bold, paddingEach { top = 14, bottom = 12, left = 0, right = 0 } ]
                            -- NOTE: weatherCode can come as null in the JSON
                            (text
                                (closestHourly.weatherCode
                                    |> Maybe.map
                                        (\l ->
                                            case model.language of
                                                English ->
                                                    l |> wmoCodeToString

                                                Spanish ->
                                                    l |> esWmoCodeToString
                                        )
                                    |> Maybe.withDefault ""
                                )
                            )
                        , el [ width fill, Font.center, Font.size 182, Font.medium ] (text (actualTemp ++ "°"))
                        ]

                Nothing ->
                    el [ width fill, Font.center, Font.size 182, Font.medium ] (text "--")

        statCards : Element MainScreenMsg
        statCards =
            let
                windCard v =
                    statCard model.primaryColor
                        Icons.air
                        (Localizations.wind model.language)
                        v

                humidityCard v =
                    statCard model.primaryColor
                        Icons.water_drop
                        (Localizations.humidity model.language)
                        v

                visibilityCard v =
                    statCard model.primaryColor
                        Icons.visibility
                        (Localizations.visibility model.language)
                        v
            in
            el
                [ padding 15
                , width fill
                ]
                (row
                    [ Font.color white
                    , Background.color model.secondaryColor
                    , width fill
                    , padding 30
                    , Border.rounded 12
                    , spaceEvenly
                    ]
                    (case hasHourlyDataOfToday of
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
                [ -- Refresh Button
                  button
                    [ padding 15
                    , Font.color model.secondaryColor
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
                    , onPress =
                        case model.currentRefetchingStatus of
                            NotRefetching ->
                                Just RefetchDataOnBackground

                            Refetching ->
                                Nothing

                            Error _ ->
                                Just RefetchDataOnBackground
                    }

                -- current country / state / city
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
                    (case model.currentAddress of
                        Just address ->
                            let
                                title val =
                                    paragraph
                                        [ centerX
                                        , centerY
                                        , Font.heavy
                                        , Font.size 28
                                        , Font.center
                                        ]
                                        [ text val ]

                                subtitle val =
                                    paragraph
                                        [ centerX
                                        , centerY
                                        , Font.center
                                        ]
                                        [ text val ]
                            in
                            case address.state of
                                Just state ->
                                    [ title address.country
                                    , subtitle state
                                    ]

                                Nothing ->
                                    case address.city of
                                        Just city ->
                                            [ title address.country
                                            , subtitle city
                                            ]

                                        Nothing ->
                                            [ title address.country ]

                        Nothing ->
                            [ none ]
                    )

                -- Menu button
                , button
                    [ padding 15
                    , Font.color model.secondaryColor
                    , Font.heavy
                    , Font.center
                    ]
                    { label = Icons.menu 28 Inherit |> Element.html, onPress = Just OpenOptionsMenu }
                ]
            , currentDateChip
            , bigCurrentTemperature
            , dailySummary
            , statCards
            , -- Weekly Forecast
              el
                [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                , Font.heavy
                ]
                (text (Localizations.weeklyForecast model.language))
            , el
                [ width fill
                ]
                (row
                    [ padding 15
                    , spacing 18
                    , width fill
                    , scrollbarX
                    ]
                    -- NOTE: daily could be made into an Nonempty list
                    -- don't know if it would be worth it
                    (List.map (\( date, code, max ) -> weeklyForecastCard date max code) apiData.daily)
                )

            -- Attribution
            , paragraph [ Font.alignRight, paddingEach { bottom = 0, top = 8, left = 0, right = 8 } ]
                [ text
                    (Localizations.attribution model.language
                        ++ " "
                    )
                , link [ Font.family [ Font.monospace ], Font.color (rgb 0 0 1) ] { label = text "Open-Meteo.com", url = "https://open-meteo.com/" }
                ]
            ]
        )


themeSelectorScreen : Language -> Color -> Color -> Element Msg
themeSelectorScreen language modelPrimaryColor modelSecondaryColor =
    let
        divider =
            el [ width fill, height (px 1), Background.color modelPrimaryColor ] none

        demoCard primaryColor secondaryColor =
            let
                primary =
                    hexToColor primaryColor |> Result.withDefault modelPrimaryColor

                secondary =
                    hexToColor secondaryColor |> Result.withDefault modelSecondaryColor
            in
            el [ padding 8, width fill ]
                (column []
                    [ row
                        [ width fill
                        , Background.color primary
                        , padding 15
                        , spacing 8
                        , Font.color secondary
                        , Border.width 1
                        , Border.color modelSecondaryColor
                        , Border.rounded 8
                        ]
                        [ column
                            [ width fill ]
                            [ paragraph [ Font.size 42, Font.heavy, paddingBottom 18 ] [ text "21°" ]
                            , paragraph [ Font.heavy, width fill, paddingBottom 8 ] [ text "Daily Summary" ]
                            , paragraph [ Font.size 16, width fill ]
                                [ text "Now it feels like 33.4°, it's actually 31.9°"
                                ]
                            ]
                        , el [ Background.color secondary, Border.rounded 12, padding 12 ]
                            (statCard primary
                                Icons.visibility
                                (Localizations.visibility language)
                                "25km/h"
                            )
                        ]
                    ]
                )
    in
    column [ width fill, Background.color modelSecondaryColor, height fill ]
        [ row
            [ width fill
            , height (px 52)
            ]
            [ button
                [ height fill
                , Font.color modelPrimaryColor
                , paddingX 8
                ]
                { label = el [ centerX, centerY ] (Icons.chevron_left 40 Inherit |> Element.html)
                , onPress = Just (OnMainScreenMsg CloseThemeSelectorScreen)
                }
            , el [ width fill, Font.alignRight, Font.color modelPrimaryColor, Font.bold, paddingRight 15 ] (text (Localizations.theme language))
            ]
        , divider
        , column [ width fill, height fill, scrollbarY ]
            [ demoCard "2A2D34" "30C5FF"
            , demoCard "780116" "F7B538"
            , demoCard "9bbc0f" "0f380f"
            ]
        ]


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
        [ Border.color defaultSecondary
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
statCard color icon title value =
    column
        [ spacing 12
        , Font.color color
        ]
        [ el [ centerX, paddingBottom 8, Font.color color ] (icon 52 Inherit |> Element.html)
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
