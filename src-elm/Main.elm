module Main exposing (..)

import Animator
import Api.Address exposing (Address)
import Api.Weather exposing (WeatherData)
import Browser
import Chart as C
import Chart.Attributes as CA
import Cmd.Extra exposing (pure)
import Components exposing (..)
import Dict
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, alpha, centerX, centerY, column, el, fill, height, inFront, layout, link, maximum, none, padding, paddingEach, paddingXY, paragraph, px, rgb, rotate, row, scrollbarX, spaceEvenly, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Flags
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html)
import Http
import Json.Decode as JD
import List.Nonempty as NEList exposing (Nonempty(..))
import Localizations exposing (Language(..))
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Ports
import Screens.ThemePicker as ThemePicker
import Screens.Welcome as Welcome
import Time exposing (Posix, Zone)
import TimeZone
import Utils exposing (..)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        MainScreen modelData ->
            Sub.batch
                [ Ports.locationReceiver ReceivedGeoLocation
                , Ports.errorObtainingCurrentPosition RequestLocationPermsApiError
                , Ports.noGeoLocationApiAvailableReceiver (\_ -> NoGeoLocationApi)
                , Ports.wentOffline (\_ -> WentOffline)
                , Ports.wentOnline (\_ -> WentOnline)
                , animator |> Animator.toSubscription Tick modelData
                ]
                |> Sub.map OnMainScreenMsg

        LoadingScreen _ ->
            Sub.none

        ThemePickerScreen m ->
            ThemePicker.themePickerSubscriptions m
                |> Sub.map OnThemePickerScreenMsg

        WelcomeScreen m ->
            Welcome.welcomeScreenSubscriptions m
                |> Sub.map OnWelcomeScreenMsg



-- MODEL


type Menu a
    = Open a
    | Closed


type RefetchingStatus a
    = NotRefetching
    | Refetching
    | Error


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


type alias EnteringManualCoordinates =
    { latitude : String
    , longitude : String
    , error : Maybe Localizations.LatAndLongManualError
    }


type alias MainScreenModel =
    { currentRefetchingAnim : Animator.Timeline (RefetchingStatus Http.Error)
    , currentRefetchingStatus : RefetchingStatus Http.Error
    , primaryColor : Color
    , secondaryColor : Color
    , optionsMenu : Menu ( Maybe EnteringManualCoordinates, Maybe String )
    , location : Location
    , zone : Zone
    , language : Language
    , customThemes : Maybe (Nonempty Theme)
    , isOnline : Bool
    , geolocationApiError : Maybe GeoLocationApiError

    -- NOTE: when I fetch I return response and current time posix
    -- they're synced as I don't need to use posix anywhere else
    -- but when I get the data and to do things at the time I fetched it
    , apiData : ( WeatherData, Posix )

    -- NOTE: could be made into a Loading | Loaded | Error type union
    -- can't be bothered though
    , currentAddress : Maybe Address
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
    | ThemePickerScreen ThemePicker.ThemePickerModel



-- MESSAGE


type LoadingScreenMsg
    = RetryFetchingWeather
    | GotWeatherResponse (Result Http.Error ( WeatherData, Posix, Zone ))


type MainScreenMsg
    = RefetchDataOnBackground
    | GotRefetchingWeatherResp (Result Http.Error ( WeatherData, Posix, Zone ))
    | Tick Time.Posix
    | GotCountryAndStateMainScreen (Result Http.Error Address)
    | ReceivedGeoLocation { latitude : Float, longitude : Float }
    | GoToThemePickerScreen
    | WentOnline
    | WentOffline
    | CloseErrorMessage
      -- Options menu
    | OpenOptionsMenu
    | CloseOptionsMenu
    | ToggleGeoLocation
    | RequestLocationPermsApiError Int
    | NoGeoLocationApi
    | ShowManualCoordinatesForm
    | OnChangeLatitude String
    | OnChangeLongitude String
    | SubmitManualLocationForm
    | CancelManualForm
    | ToggleLanguage


type Msg
    = OnWelcomeScreenMsg Welcome.WelcomeScreenMsg
    | OnLoadingScreenMsg LoadingScreenMsg
    | OnMainScreenMsg MainScreenMsg
    | OnThemePickerScreenMsg ThemePicker.ThemePickerMsg



-- MAIN


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
    case Flags.flagsDecoders val of
        Ok flags ->
            let
                mainDefaults :
                    { apiData : ( WeatherData, Posix )
                    , currentAddress : Maybe Address
                    , customThemes : Maybe (Nonempty Theme)
                    , language : Language
                    , location : Location
                    , primaryColor : Color
                    , secondaryColor : Color
                    , zone : Time.Zone
                    , countryAndStateVisibility : Animator.Timeline Bool
                    }
                    -> MainScreenModel
                mainDefaults { apiData, currentAddress, customThemes, language, location, primaryColor, secondaryColor, zone, countryAndStateVisibility } =
                    { apiData = apiData
                    , currentAddress = currentAddress
                    , customThemes = customThemes
                    , language = language
                    , location = location
                    , primaryColor = primaryColor
                    , secondaryColor = secondaryColor
                    , zone = zone
                    , countryAndStateVisibility = countryAndStateVisibility

                    --
                    , geolocationApiError = Nothing
                    , currentRefetchingStatus = Refetching
                    , currentRefetchingAnim = Animator.init Refetching
                    , optionsMenu = Closed

                    --  NOTE: JS immediately checks if online or offline
                    , isOnline = True
                    }
            in
            case flags of
                Flags.CachedWeatherAndAddressData { cachedWeatherData, posixTimeNow, addressData, usingGeoLocation, language, theme, customThemes, timezone } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData

                        primaryColor : Color
                        primaryColor =
                            theme |> Maybe.map Tuple.first |> Maybe.withDefault defaultPrimary

                        secondaryColor : Color
                        secondaryColor =
                            theme |> Maybe.map Tuple.second |> Maybe.withDefault defaultSecondary
                    in
                    ( { apiData = ( cachedWeatherData, Time.millisToPosix posixTimeNow )
                      , language = language
                      , location =
                            if usingGeoLocation then
                                UsingGeoLocation { latitude = latitude, longitude = longitude }

                            else
                                FixedCoordinates { latitude = latitude, longitude = longitude }
                      , primaryColor = primaryColor
                      , secondaryColor = secondaryColor
                      , currentAddress = Just addressData
                      , countryAndStateVisibility = Animator.init True
                      , customThemes = customThemes
                      , zone =
                            TimeZone.zones
                                |> Dict.get timezone
                                |> Maybe.map (\l -> l ())
                                |> Maybe.withDefault Time.utc
                      }
                        |> mainDefaults
                    , if usingGeoLocation then
                        Ports.requestLoc

                      else
                        Cmd.batch
                            [ Api.Address.getAddress { latitude = latitude, longitude = longitude } GotCountryAndStateMainScreen
                            , Api.Weather.getWeather { latitude = latitude, longitude = longitude } GotRefetchingWeatherResp
                            ]
                    )
                        |> mapToMainScreen

                Flags.CachedWeatherData { cachedWeatherData, posixTimeNow, usingGeoLocation, language, theme, customThemes, timezone } ->
                    let
                        { latitude, longitude } =
                            cachedWeatherData

                        primaryColor : Color
                        primaryColor =
                            theme |> Maybe.map Tuple.first |> Maybe.withDefault defaultPrimary

                        secondaryColor : Color
                        secondaryColor =
                            theme |> Maybe.map Tuple.second |> Maybe.withDefault defaultSecondary
                    in
                    ( { apiData = ( cachedWeatherData, Time.millisToPosix posixTimeNow )
                      , language = language
                      , location =
                            if usingGeoLocation then
                                UsingGeoLocation { latitude = latitude, longitude = longitude }

                            else
                                FixedCoordinates { latitude = latitude, longitude = longitude }
                      , primaryColor = primaryColor
                      , secondaryColor = secondaryColor
                      , customThemes = customThemes
                      , countryAndStateVisibility = Animator.init False
                      , currentAddress = Nothing
                      , zone =
                            TimeZone.zones
                                |> Dict.get timezone
                                |> Maybe.map (\l -> l ())
                                |> Maybe.withDefault Time.utc
                      }
                        |> mainDefaults
                    , if usingGeoLocation then
                        Ports.requestLoc

                      else
                        Cmd.batch
                            [ Api.Address.getAddress { latitude = latitude, longitude = longitude } GotCountryAndStateMainScreen
                            , Api.Weather.getWeather { latitude = latitude, longitude = longitude } GotRefetchingWeatherResp
                            ]
                    )
                        |> mapToMainScreen

                Flags.Initial language ->
                    WelcomeScreen (Welcome.welcomeScreenInit language) |> pure

        Err _ ->
            -- NOTE: this will never happen unless the flags are screwed up
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
                                , Api.Weather.getWeather coords GotWeatherResponse |> Cmd.map OnLoadingScreenMsg
                                  -- NOTE: Api.GetAddress.getAddress could be called here
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
                                , customThemes = Nothing
                                , geolocationApiError = Nothing
                                , zone = zone
                                , location =
                                    if model.isUsingGeoLocation then
                                        UsingGeoLocation model.coordinates

                                    else
                                        FixedCoordinates model.coordinates
                                , primaryColor = defaultPrimary
                                , secondaryColor = defaultSecondary
                                , optionsMenu = Closed
                                , currentAddress = Nothing
                                , countryAndStateVisibility = Animator.init False
                                , isOnline = True
                                }
                            , Api.Address.getAddress { latitude = data.latitude, longitude = data.longitude } GotCountryAndStateMainScreen
                                |> Cmd.map OnMainScreenMsg
                            )

                        Err err ->
                            LoadingScreen { model | fetchingStatus = Failure err } |> pure

                RetryFetchingWeather ->
                    ( LoadingScreen { model | fetchingStatus = Loading }
                    , Api.Weather.getWeather model.coordinates GotWeatherResponse
                        |> Cmd.map OnLoadingScreenMsg
                    )

        ( OnMainScreenMsg msg, MainScreen model ) ->
            case msg of
                Tick newTime ->
                    (model |> Animator.update newTime animator)
                        |> pure
                        |> mapToMainScreen

                WentOnline ->
                    { model | isOnline = True }
                        |> pure
                        |> mapToMainScreen

                WentOffline ->
                    { model | isOnline = False }
                        |> pure
                        |> mapToMainScreen

                CloseErrorMessage ->
                    { model | geolocationApiError = Nothing }
                        |> pure
                        |> mapToMainScreen

                -- Options menu
                GoToThemePickerScreen ->
                    ThemePicker.themePickerInit model.language ( model.primaryColor, model.secondaryColor ) model.zone model.location model.apiData model.currentAddress model.customThemes
                        |> pure
                        |> (\( a, b ) -> ( ThemePickerScreen a, b |> Cmd.map OnThemePickerScreenMsg ))

                ToggleLanguage ->
                    ( { model
                        | language =
                            case model.language of
                                English ->
                                    Spanish

                                Spanish ->
                                    English
                      }
                    , Ports.userSetLanguage model.language
                    )
                        |> mapToMainScreen

                OpenOptionsMenu ->
                    { model | optionsMenu = Open ( Nothing, Nothing ) }
                        |> pure
                        |> mapToMainScreen

                CloseOptionsMenu ->
                    { model | optionsMenu = Closed }
                        |> pure
                        |> mapToMainScreen

                ShowManualCoordinatesForm ->
                    case model.location of
                        UsingGeoLocation coords ->
                            { model
                                | optionsMenu =
                                    Open
                                        ( Just
                                            { latitude = coords.latitude |> String.fromFloat
                                            , longitude = coords.longitude |> String.fromFloat
                                            , error = Nothing
                                            }
                                        , Nothing
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        FixedCoordinates coords ->
                            { model
                                | optionsMenu =
                                    Open
                                        ( Just
                                            { latitude = coords.latitude |> String.fromFloat
                                            , longitude = coords.longitude |> String.fromFloat
                                            , error = Nothing
                                            }
                                        , Nothing
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                OnChangeLatitude newLatitude ->
                    case model.optionsMenu of
                        Open ( Just isEditingCoordinatesManually, err ) ->
                            { model
                                | optionsMenu =
                                    Open
                                        ( Just
                                            { isEditingCoordinatesManually
                                                | latitude = newLatitude
                                            }
                                        , err
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                OnChangeLongitude newLongitude ->
                    case model.optionsMenu of
                        Open ( Just isEditingCoordinatesManually, err ) ->
                            { model
                                | optionsMenu =
                                    Open
                                        ( Just
                                            { isEditingCoordinatesManually
                                                | longitude = newLongitude
                                            }
                                        , err
                                        )
                            }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                CancelManualForm ->
                    case model.optionsMenu of
                        Open ( Just _, err ) ->
                            { model | optionsMenu = Open ( Nothing, err ) }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                SubmitManualLocationForm ->
                    case model.optionsMenu of
                        Open ( Just manualCoordinates, err ) ->
                            let
                                { latitude, longitude } =
                                    manualCoordinates

                                setManualLocationError : Maybe Localizations.LatAndLongManualError -> ( Model, Cmd Msg )
                                setManualLocationError error =
                                    { model
                                        | optionsMenu =
                                            Open
                                                ( Just
                                                    { manualCoordinates
                                                        | error = error
                                                    }
                                                , err
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
                                                        , optionsMenu = Open ( Nothing, Nothing )
                                                      }
                                                    , Cmd.batch
                                                        [ Api.Address.getAddress { latitude = latFloat, longitude = lonFloat } GotCountryAndStateMainScreen
                                                        , Api.Weather.getWeather { latitude = latFloat, longitude = lonFloat } GotRefetchingWeatherResp
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
                            ( { model | location = FixedCoordinates fixedCoordinates }, Ports.notUsingGeo )
                                |> mapToMainScreen

                        FixedCoordinates _ ->
                            case model.optionsMenu of
                                Open ( manualCoords, _ ) ->
                                    ( { model | optionsMenu = Open ( manualCoords, Nothing ) }, Ports.requestLoc )
                                        |> mapToMainScreen

                                _ ->
                                    ( model, Ports.requestLoc )
                                        |> mapToMainScreen

                RequestLocationPermsApiError err ->
                    case model.optionsMenu of
                        Open ( manuallyCoords, _ ) ->
                            { model | optionsMenu = Open ( manuallyCoords, Just (Localizations.geoLocationApiError model.language (codeToGeoLocationApiError err) ++ ".") ) }
                                |> pure
                                |> mapToMainScreen

                        Closed ->
                            { model
                                | geolocationApiError = Just (codeToGeoLocationApiError err)
                                , currentRefetchingStatus = Error
                                , currentRefetchingAnim = Animator.init Error
                            }
                                |> pure
                                |> mapToMainScreen

                NoGeoLocationApi ->
                    case model.optionsMenu of
                        Open ( manuallyCoords, _ ) ->
                            { model | optionsMenu = Open ( manuallyCoords, Just (Localizations.deviceDoesNotSupportGeolocation model.language ++ ".") ) }
                                |> pure
                                |> mapToMainScreen

                        _ ->
                            model
                                |> pure
                                |> mapToMainScreen

                -- Background refetching
                GotCountryAndStateMainScreen countryAndState ->
                    case countryAndState of
                        Ok address ->
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
                            (if model.isOnline then
                                { model
                                    | currentAddress = Nothing
                                    , countryAndStateVisibility =
                                        model.countryAndStateVisibility
                                            |> Animator.go Animator.slowly False
                                }
                                    |> pure

                             else
                                model |> pure
                            )
                                |> mapToMainScreen

                RefetchDataOnBackground ->
                    ( { model
                        | currentRefetchingAnim =
                            model.currentRefetchingAnim
                                |> Animator.go Animator.immediately Refetching
                        , currentRefetchingStatus = Refetching
                        , geolocationApiError = Nothing
                      }
                    , case model.location of
                        UsingGeoLocation _ ->
                            Ports.requestLoc

                        FixedCoordinates coords ->
                            Cmd.batch
                                [ Api.Address.getAddress coords GotCountryAndStateMainScreen
                                , Api.Weather.getWeather coords GotRefetchingWeatherResp
                                ]
                    )
                        |> mapToMainScreen

                ReceivedGeoLocation coords ->
                    case model.optionsMenu of
                        Open _ ->
                            ( { model | optionsMenu = Open ( Nothing, Nothing ), location = UsingGeoLocation coords }
                            , Cmd.batch
                                [ Api.Address.getAddress coords GotCountryAndStateMainScreen
                                , Api.Weather.getWeather coords GotRefetchingWeatherResp
                                ]
                            )
                                |> mapToMainScreen

                        _ ->
                            ( { model | location = UsingGeoLocation coords }
                            , Cmd.batch
                                [ Api.Address.getAddress coords GotCountryAndStateMainScreen
                                , Api.Weather.getWeather coords GotRefetchingWeatherResp
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
                                , zone = zone
                            }

                        Err _ ->
                            { model
                                | currentRefetchingAnim =
                                    model.currentRefetchingAnim
                                        |> Animator.go Animator.immediately Error
                                , currentRefetchingStatus = Error
                            }
                    )
                        |> pure
                        |> mapToMainScreen

        ( OnThemePickerScreenMsg ms, ThemePickerScreen md ) ->
            ThemePicker.themePickerUpdate ms md
                |> (\( a, b ) ->
                        if a.exitScreen then
                            let
                                ( primary, secondary ) =
                                    a.currentTheme
                            in
                            ( MainScreen
                                { apiData = a.apiData
                                , currentRefetchingStatus = NotRefetching
                                , currentRefetchingAnim = Animator.init NotRefetching
                                , language = a.language
                                , location = a.location
                                , primaryColor = primary
                                , customThemes = a.customThemes
                                , secondaryColor = secondary
                                , optionsMenu = Closed
                                , geolocationApiError = Nothing
                                , currentAddress = a.currentAddress
                                , countryAndStateVisibility = Animator.init True
                                , zone = a.zone

                                -- NOTE: immediately check
                                , isOnline = True
                                }
                            , Ports.checkIfOnline
                            )

                        else
                            ( ThemePickerScreen a, b |> Cmd.map OnThemePickerScreenMsg )
                   )

        _ ->
            topModel |> pure



-- VIEW


view : Model -> Html Msg
view model =
    let
        maxAppWidth =
            550
    in
    layout
        [ Font.family
            [ Font.typeface "Inter"
            , Font.sansSerif
            ]
        , width fill
        , inFront
            (case model of
                MainScreen modelData ->
                    el [ centerX, width (fill |> maximum maxAppWidth), height fill, noPointerEvents ]
                        (column [ width fill, noPointerEvents, height fill ]
                            [ el [ autoPointerEvents, width fill, alignTop ]
                                (case modelData.optionsMenu of
                                    Open ( isEnteringManualCoordinates, geoApiError ) ->
                                        let
                                            divider : Element msg
                                            divider =
                                                el [ width fill, height (px 1), Background.color modelData.primaryColor ] none
                                        in
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
                                                , onPress = Just GoToThemePickerScreen
                                                }
                                            , divider

                                            -- Geo location
                                            , case geoApiError of
                                                Just err ->
                                                    paragraph
                                                        [ width fill
                                                        , padding 15
                                                        , Font.medium
                                                        , Font.color modelData.secondaryColor
                                                        , Background.color modelData.primaryColor
                                                        ]
                                                        [ el [ Font.heavy ] (text "Error: ")
                                                        , text err
                                                        ]

                                                Nothing ->
                                                    none
                                            , button [ width fill ]
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
                                                            (text (Localizations.geolocation modelData.language))
                                                        , Components.toggle
                                                            { primaryColor = modelData.primaryColor
                                                            , secondaryColor = modelData.secondaryColor
                                                            , isToggled =
                                                                case modelData.location of
                                                                    UsingGeoLocation _ ->
                                                                        True

                                                                    FixedCoordinates _ ->
                                                                        False
                                                            , values = ( "ON", "OFF" )
                                                            }
                                                        ]
                                                , onPress = Just ToggleGeoLocation
                                                }
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
                                                            , Font.color modelData.secondaryColor
                                                            ]
                                                            { label =
                                                                row []
                                                                    [ el [ paddingXY 5 5 ] (text (format usLocale coordinates.latitude ++ ", " ++ format usLocale coordinates.longitude))
                                                                    , el [ width (px 2), height fill, Background.color modelData.secondaryColor ] none
                                                                    , el [ paddingXY 5 5 ] (Icons.edit 22 Inherit |> Element.html)
                                                                    ]
                                                            , onPress = Just ShowManualCoordinatesForm
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
                                                                , Font.color modelData.secondaryColor
                                                                ]
                                                                { onChange = OnChangeLatitude
                                                                , text = manualCoordinates.latitude
                                                                , placeholder = Just (Input.placeholder [] (el [ Font.color modelData.secondaryColor, alpha 0.65 ] (text manualCoordinates.latitude)))
                                                                , label = Input.labelAbove [ Font.color modelData.primaryColor ] (text (Localizations.latitude modelData.language ++ ":"))
                                                                }
                                                            , Input.text
                                                                [ width fill
                                                                , Background.color modelData.primaryColor
                                                                , Font.color modelData.secondaryColor
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
                                            , button [ width fill ]
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
                                                            (text (Localizations.languagePicker modelData.language))
                                                        , Components.toggle
                                                            { primaryColor = modelData.primaryColor
                                                            , secondaryColor = modelData.secondaryColor
                                                            , isToggled = modelData.language == English
                                                            , values = ( "EN", "ES" )
                                                            }
                                                        ]
                                                , onPress = Just ToggleLanguage
                                                }
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
                            , el [ autoPointerEvents, width fill, alignBottom ]
                                (if modelData.isOnline then
                                    case modelData.geolocationApiError of
                                        Just err ->
                                            column []
                                                [ el [ width fill, height (px 2), Background.color modelData.primaryColor ] none
                                                , row [ width fill, Background.color modelData.secondaryColor, Font.size 21, Font.bold, Font.color modelData.primaryColor ]
                                                    [ paragraph [ padding 8 ]
                                                        [ el [ Font.heavy ] (text "Error: "), text (Localizations.geoLocationApiError modelData.language err) ]
                                                    , button [ height fill, paddingX 12 ]
                                                        { label =
                                                            el [ centerX ]
                                                                (Icons.close 28 Inherit
                                                                    |> Element.html
                                                                )
                                                        , onPress = Just CloseErrorMessage
                                                        }
                                                    ]
                                                ]

                                        Nothing ->
                                            none

                                 else
                                    row [ width fill, Background.color modelData.secondaryColor, padding 8, Font.size 21, Font.bold ]
                                        [ paragraph []
                                            [ text (Localizations.noInternet modelData.language) ]
                                        , Icons.wifi_off 28 Inherit
                                            |> Element.html
                                        ]
                                )
                            ]
                        )
                        |> Element.map OnMainScreenMsg

                ThemePickerScreen _ ->
                    none

                LoadingScreen _ ->
                    none

                WelcomeScreen noLocationDataModel ->
                    if noLocationDataModel.geoLocationApiError /= "" then
                        el [ centerX, width (fill |> maximum maxAppWidth) ]
                            (paragraph
                                [ Background.color defaultSecondary
                                , Font.color defaultPrimary
                                , Font.bold
                                , paddingXY 24 12
                                , Font.size 22
                                ]
                                [ text "Error: ", el [ Font.light ] (text noLocationDataModel.geoLocationApiError) ]
                            )

                    else
                        none
            )
        ]
        (el
            [ centerX, width (fill |> maximum maxAppWidth), height fill ]
            (case model of
                WelcomeScreen m ->
                    Welcome.welcomeScreenView m
                        |> Element.map OnWelcomeScreenMsg

                LoadingScreen m ->
                    loadingScreenView m

                MainScreen m ->
                    mainScreen m |> Element.map OnMainScreenMsg

                ThemePickerScreen m ->
                    ThemePicker.themePickerView m |> Element.map OnThemePickerScreenMsg
            )
        )


loadingScreenView : LoadingScreenModel -> Element Msg
loadingScreenView { fetchingStatus, language } =
    el
        [ width fill
        , height fill
        , Background.color defaultPrimary
        , paddingEach { top = 15, bottom = 16, left = 0, right = 0 }
        ]
        (case fetchingStatus of
            Loading ->
                initialLoadingScreen language

            Failure err ->
                column [ centerX, centerY ]
                    [ paragraph [ Font.center, Font.size 54, Font.semiBold ]
                        [ el [ Font.center ] (text "Error:")
                        , br
                        , el [ Font.heavy ] (text (Localizations.httpError language err))
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



-- NOTE: currently showing data for the most recent hour
-- i.e: Current temperature, wind, humidity, visibility, etc.


mainScreen : MainScreenModel -> Element MainScreenMsg
mainScreen ({ zone } as model) =
    let
        hasHourlyDataOfToday : Maybe (Nonempty Api.Weather.Hourly)
        hasHourlyDataOfToday =
            apiData.hourly |> hourlyDataOfToday zone currentTime |> NEList.fromList

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

                    day : String
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
                    column
                        [ Font.color model.secondaryColor ]
                        [ el
                            [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                            , Font.heavy
                            ]
                            (text (Localizations.dailySummary model.language))
                        , let
                            closestHourly : Api.Weather.Hourly
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
                        closestHourly : Api.Weather.Hourly
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
                    column [ width fill, Font.color model.secondaryColor ]
                        [ el [ width fill, Font.center, Font.bold, paddingEach { top = 14, bottom = 12, left = 0, right = 0 } ]
                            -- NOTE: weatherCode can come as null in the JSON
                            (text
                                (closestHourly.weatherCode
                                    |> Maybe.map
                                        (\l ->
                                            case model.language of
                                                English ->
                                                    l |> Api.Weather.wmoCodeToString

                                                Spanish ->
                                                    l |> Api.Weather.esWmoCodeToString
                                        )
                                    |> Maybe.withDefault ""
                                )
                            )
                        , el
                            [ width fill
                            , Font.center
                            , if String.length actualTemp < 2 && not (String.contains "-" actualTemp) then
                                Font.size 180

                              else if String.length actualTemp < 3 && String.contains "-" actualTemp then
                                Font.size 140

                              else if String.contains "-" actualTemp then
                                Font.size 115

                              else
                                Font.size 140
                            , Font.medium
                            ]
                            (text (actualTemp ++ "°"))
                        ]

                Nothing ->
                    el [ width fill, Font.center, Font.size 180, Font.medium ] (text "--")

        statCards : Element MainScreenMsg
        statCards =
            let
                windCard : String -> Element msg
                windCard v =
                    statCard model.primaryColor
                        Icons.air
                        (Localizations.wind model.language)
                        v

                humidityCard : String -> Element msg
                humidityCard v =
                    statCard model.primaryColor
                        Icons.water_drop
                        (Localizations.humidity model.language)
                        v

                visibilityCard : String -> Element msg
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
                                hourlyClosestToMine : Api.Weather.Hourly
                                hourlyClosestToMine =
                                    timeClosestToMine zone currentTime firstHourly restHourly
                            in
                            [ windCard
                                (hourlyClosestToMine
                                    |> .windSpeed
                                    -- NOTE: it can come as null in the JSON
                                    |> Maybe.map (round >> String.fromInt >> prepend "km/h")
                                    |> Maybe.withDefault "--"
                                )
                            , humidityCard
                                (hourlyClosestToMine
                                    |> .relativeHumidity
                                    |> String.fromInt
                                    |> prepend "%"
                                )
                            , visibilityCard
                                (hourlyClosestToMine
                                    |> .visibility
                                    |> toKm
                                    |> round
                                    |> String.fromInt
                                    |> prepend "km"
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
        , paddingEach { top = 0, bottom = 16, left = 0, right = 0 }
        ]
        (column
            [ width fill
            , height fill
            ]
            [ row [ width fill ]
                [ -- Refresh Button
                  button
                    [ padding 18
                    , Font.color model.secondaryColor
                    , Font.heavy
                    , Font.center
                    , alignLeft
                    , alignTop
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

                                            Error ->
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

                                Error ->
                                    Icons.sync_problem 28 Inherit
                                        |> Element.html
                            )
                    , onPress =
                        case model.currentRefetchingStatus of
                            NotRefetching ->
                                Just RefetchDataOnBackground

                            Refetching ->
                                Nothing

                            Error ->
                                Just RefetchDataOnBackground
                    }

                -- current country / state / city
                , case model.currentAddress of
                    Just address ->
                        let
                            title : String -> Element msg
                            title val =
                                paragraph
                                    [ centerX
                                    , centerY
                                    , Font.heavy
                                    , Font.size 28
                                    , Font.center
                                    ]
                                    [ text val ]

                            subtitle : String -> Element msg
                            subtitle val =
                                paragraph
                                    [ centerX
                                    , centerY
                                    , Font.center
                                    ]
                                    [ text val ]
                        in
                        column
                            [ Font.color model.secondaryColor
                            , width fill
                            , spacing 6
                            , paddingY 15
                            , alpha
                                (Animator.move model.countryAndStateVisibility <|
                                    \state ->
                                        if state then
                                            Animator.at 1

                                        else
                                            Animator.at 0
                                )
                            ]
                            (case address.state of
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
                            )

                    Nothing ->
                        none

                -- Menu button
                , button
                    [ padding 18
                    , Font.color model.secondaryColor
                    , Font.heavy
                    , alignRight
                    , alignTop
                    , Font.center
                    ]
                    { label = Icons.menu 28 Inherit |> Element.html, onPress = Just OpenOptionsMenu }
                ]
            , currentDateChip
            , bigCurrentTemperature
            , dailySummary
            , case hasHourlyDataOfToday of
                Just val ->
                    let
                        pColor =
                            toRgb model.secondaryColor
                                |> (\{ blue, green, red } ->
                                        List.map toHex
                                            [ round (red * 255)
                                            , round (green * 255)
                                            , round (blue * 255)

                                            -- don't know how to do alpha so i'm just omitting it here
                                            -- , round (alpha * 255)
                                            ]
                                            |> (::) "#"
                                            |> String.concat
                                   )

                        data : List { x : Float, y : Maybe Float }
                        data =
                            val
                                |> NEList.toList
                                |> List.map
                                    (\hourly ->
                                        { x = hourly.time |> Time.toHour model.zone |> toFloat
                                        , y = hourly.temperature
                                        }
                                    )

                        lowestTemp =
                            data
                                |> List.map .y
                                |> List.map (Maybe.withDefault 0)
                                |> List.minimum
                                |> Maybe.map (\l -> l - 1)
                                |> Maybe.withDefault 0

                        highestTemp =
                            data
                                |> List.map .y
                                |> List.map (Maybe.withDefault 0)
                                |> List.maximum
                                |> Maybe.map ((+) 1)
                                |> Maybe.withDefault 0
                    in
                    el [ width fill, paddingEach { right = 18, top = 32, bottom = 32, left = 50 } ]
                        (C.chart
                            [ CA.domain
                                [ CA.lowest lowestTemp CA.exactly
                                , CA.highest highestTemp CA.exactly
                                ]
                            ]
                            [ C.xLabels
                                [ CA.color pColor
                                , CA.ints
                                , CA.pinned .min
                                ]
                            , C.yLabels
                                [ CA.color pColor
                                , CA.ints
                                ]
                            , C.series .x
                                [ C.interpolatedMaybe .y [ CA.color pColor ] []
                                ]
                                data
                            ]
                            |> Element.html
                        )

                Nothing ->
                    none
            , -- Weekly Forecast
              el
                [ paddingEach { top = 15, left = 15, right = 0, bottom = 0 }
                , Font.heavy
                , Font.color model.secondaryColor
                ]
                (text (Localizations.weeklyForecast model.language))
            , el
                [ width fill
                , Font.color model.secondaryColor
                ]
                (row
                    [ padding 15
                    , spacing 18
                    , width fill
                    , scrollbarX
                    ]
                    -- NOTE: daily could be made into an Nonempty list
                    -- don't know if it would be worth it
                    (List.map (\( date, code, max ) -> weeklyForecastCard model.secondaryColor date max code) apiData.daily)
                )
            , statCards

            -- Attribution
            , column [ width fill, spacing 8 ]
                [ paragraph [ Font.alignRight, paddingEach { bottom = 0, top = 8, left = 8, right = 8 }, width fill ]
                    [ el
                        [ Font.color model.secondaryColor ]
                        (text (Localizations.attribution model.language ++ " "))
                    , link [ Font.family [ Font.monospace ], Font.underline, Font.color model.primaryColor, Background.color model.secondaryColor, paddingXY 6 2 ] { label = text "Open-Meteo", url = "https://open-meteo.com/" }
                    ]
                , paragraph [ Font.alignRight, paddingEach { bottom = 0, top = 8, left = 8, right = 8 }, width fill ]
                    [ el
                        [ Font.color model.secondaryColor ]
                        (text (Localizations.attributionAddress model.language ++ " "))
                    , link [ Font.family [ Font.monospace ], Font.underline, Font.color model.primaryColor, Background.color model.secondaryColor, paddingXY 6 2 ] { label = text "Open Street map", url = "https://www.openstreetmap.org/" }
                    ]
                ]
            ]
        )


initialLoadingScreen : Language -> Element msg
initialLoadingScreen lang =
    column
        [ centerX, centerY, width fill ]
        [ paragraph [ Font.center, paddingXY 32 0 ] [ el [ Font.bold, Font.size 64 ] (text (Localizations.loading lang)) ] ]


weeklyForecastCard : Color -> Posix -> Float -> Api.Weather.WMOCode -> Element msg
weeklyForecastCard borderColor date max code =
    let
        month : String
        month =
            Time.toMonth Time.utc date |> monthToString

        day : String
        day =
            Time.toDay Time.utc date |> String.fromInt
    in
    column
        [ Border.color borderColor
        , rounded 14
        , Border.solid
        , Border.width 3
        , padding 14
        , Font.bold
        , spacing 12
        ]
        [ el [ centerX ] (text (String.fromFloat max ++ "°"))
        , el [ centerX ] (Api.Weather.wmoCodeToIcon code 24 Inherit |> Element.html)
        , el [ centerX, Font.size 14 ] (text (day ++ nbsp ++ String.left 3 month))
        ]



-- HELPERS


hourlyDataOfToday : Zone -> Posix -> List Api.Weather.Hourly -> List Api.Weather.Hourly
hourlyDataOfToday myZone currTime list =
    let
        year : Int
        year =
            Time.toYear myZone currTime

        month : Int
        month =
            Time.toMonth myZone currTime |> monthToInt

        day : Int
        day =
            Time.toDay myZone currTime
    in
    List.filter
        (\hourly ->
            let
                hourlyYear : Int
                hourlyYear =
                    Time.toYear myZone hourly.time

                hourlyMonth : Int
                hourlyMonth =
                    Time.toMonth myZone hourly.time |> monthToInt

                hourlyDay : Int
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


timeClosestToMine : Zone -> Posix -> Api.Weather.Hourly -> List Api.Weather.Hourly -> Api.Weather.Hourly
timeClosestToMine zone time firstItem list =
    -- NOTE: could be made even more bulletproof and use a
    -- function that checks that it's a few hours before
    -- the user time, at most, and returns a Maybe Hourly instead
    let
        year : Int
        year =
            Time.toYear zone time

        month : Int
        month =
            Time.toMonth zone time |> monthToInt

        day : Int
        day =
            Time.toDay zone time

        hour : Int
        hour =
            Time.toHour zone time

        minute : Int
        minute =
            Time.toMinute zone time
    in
    List.foldl
        (\a b ->
            let
                y : Int
                y =
                    Time.toYear zone a.time

                m : Int
                m =
                    Time.toMonth zone a.time |> monthToInt

                d : Int
                d =
                    Time.toDay zone a.time

                h : Int
                h =
                    Time.toHour zone a.time

                min : Int
                min =
                    Time.toMinute zone a.time
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
