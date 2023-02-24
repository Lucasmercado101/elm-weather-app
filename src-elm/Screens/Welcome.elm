module Screens.Welcome exposing (..)

import Cmd.Extra exposing (pure)
import Element exposing (Element, alpha, centerX, centerY, column, el, fill, height, none, paddingXY, paragraph, px, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import Localizations exposing (Language(..))
import Ports exposing (..)
import Utils exposing (..)


type alias WelcomeScreenModel =
    { geoLocationApiError : String
    , language : Language

    -- manual location form
    , manualLocation : ( String, String )
    , manualLocationErr : String

    -- parent will intercept to check this
    , receivedLocation : Maybe { latitude : Float, longitude : Float }
    , usingGeoLocation : Bool
    }


type WelcomeScreenMsg
    = RequestLocationPerms
    | RequestLocationPermsApiError Int
    | NoGeoLocationApi ()
    | ReceivedGeoLocation { latitude : Float, longitude : Float }
      -- manually entering location
    | OnChangeLatitude String
    | OnChangeLongitude String
    | SubmitManualLocationForm


welcomeScreenSubscriptions : WelcomeScreenModel -> Sub WelcomeScreenMsg
welcomeScreenSubscriptions _ =
    Sub.batch
        [ errorObtainingCurrentPosition RequestLocationPermsApiError
        , locationReceiver ReceivedGeoLocation
        , noGeoLocationApiAvailableReceiver NoGeoLocationApi
        ]


welcomeScreenInit : Language -> WelcomeScreenModel
welcomeScreenInit lang =
    { geoLocationApiError = ""
    , receivedLocation = Nothing
    , usingGeoLocation = False
    , language = lang

    --
    , manualLocation = ( "", "" )
    , manualLocationErr = ""
    }


welcomeScreenUpdate : WelcomeScreenMsg -> WelcomeScreenModel -> ( WelcomeScreenModel, Cmd WelcomeScreenMsg )
welcomeScreenUpdate msg model =
    let
        ( lat, lon ) =
            model.manualLocation

        -- NOTE: parent will intercept this and move on to the next screen
        exitScreen : { latitude : Float, longitude : Float } -> Bool -> WelcomeScreenModel
        exitScreen val usingGeo =
            { model | receivedLocation = Just val, usingGeoLocation = usingGeo }

        setManualLocationError : String -> WelcomeScreenModel
        setManualLocationError errStr =
            { model | manualLocationErr = errStr }
    in
    case msg of
        ReceivedGeoLocation coords ->
            exitScreen coords True |> pure

        --------
        RequestLocationPerms ->
            ( { model | geoLocationApiError = "" }, requestLoc )

        RequestLocationPermsApiError err ->
            { model
                | geoLocationApiError =
                    err
                        |> codeToGeoLocationApiError
                        |> geoLocationApiErrorToString
            }
                |> pure

        NoGeoLocationApi () ->
            { model | geoLocationApiError = noGeoApiAvailableErrStr }
                |> pure

        --------
        OnChangeLatitude str ->
            { model | manualLocation = ( str, lon ) } |> pure

        OnChangeLongitude str ->
            { model | manualLocation = ( lat, str ) } |> pure

        SubmitManualLocationForm ->
            (case String.toFloat lat of
                Just latFloat ->
                    if latFloat < -90 || latFloat > 90 then
                        setManualLocationError "Latitude must be between -90 and 90"

                    else
                        case String.toFloat lon of
                            Just lonFloat ->
                                if lonFloat < -180 || lonFloat > 180 then
                                    setManualLocationError "Longitude must be between -180 and 180"

                                else
                                    exitScreen { latitude = latFloat, longitude = lonFloat } False

                            Nothing ->
                                setManualLocationError "Longitude must be a valid number"

                Nothing ->
                    setManualLocationError "Latitude must be a valid number"
            )
                |> pure


welcomeScreenView : WelcomeScreenModel -> Element WelcomeScreenMsg
welcomeScreenView { manualLocationErr, manualLocation } =
    let
        ( lat, lon ) =
            manualLocation
    in
    el
        [ width fill
        , height fill
        , Background.color defaultPrimary
        ]
        (column [ width fill, centerX, centerY ]
            [ paragraph [ Font.center, Font.size 52, Font.semiBold ]
                [ el [ Font.center ] (text "Welcome to")
                , br
                , el [ Font.heavy ] (text "WeatherMate")
                ]
            , el [ paddingTop 18, centerX ]
                (button
                    [ centerX
                    , Background.color defaultSecondary
                    , Font.color defaultPrimary
                    , Font.bold
                    , paddingXY 24 12
                    , Font.size 22
                    , if manualLocationErr == noGeoApiAvailableErrStr then
                        Font.strike

                      else
                        Font.bold
                    ]
                    -- NOTE: no good way to show "fetching location" message
                    { label = text "Enable Location Permission", onPress = Just RequestLocationPerms }
                )
            , el [ Font.bold, centerX, paddingXY 0 15 ] (text "or")
            , column
                [ centerX
                , Background.color defaultSecondary
                ]
                [ -- Error message
                  if manualLocationErr /= "" then
                    column [ width fill ]
                        [ paragraph
                            [ paddingXY 24 12
                            , Font.center
                            , spacing 8
                            , Font.color defaultPrimary
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
                                (text manualLocationErr)
                            ]
                        , el [ width fill, height (px 1), Background.color defaultPrimary ] none
                        ]

                  else
                    none

                -- Latitude and Longitude form
                , column
                    [ paddingXY 24 24
                    , width fill
                    , spacing 24
                    ]
                    [ Input.text
                        [ width fill
                        , Background.color defaultPrimary
                        ]
                        { onChange = \l -> OnChangeLatitude l
                        , text = lat
                        , placeholder = Just (Input.placeholder [] (el [ Font.color defaultSecondary, alpha 0.65 ] (text "-150.58")))
                        , label = Input.labelAbove [ Font.color defaultPrimary ] (text "Latitude:")
                        }
                    , Input.text
                        [ width fill
                        , Background.color defaultPrimary
                        ]
                        { onChange = \l -> OnChangeLongitude l
                        , text = lon
                        , placeholder = Just (Input.placeholder [] (el [ Font.color defaultSecondary, alpha 0.65 ] (text "75.88")))
                        , label = Input.labelAbove [ Font.color defaultPrimary ] (text "Longitude:")
                        }
                    ]
                , divider
                , button
                    [ paddingXY 24 12
                    , centerX
                    , Font.color defaultPrimary
                    , Font.bold
                    , Font.size 22
                    ]
                    { label = text "Enter coordinates manually", onPress = Just SubmitManualLocationForm }
                ]
            ]
        )


divider : Element msg
divider =
    el [ width fill, height (px 1), Background.color defaultPrimary ] none


noGeoApiAvailableErrStr : String
noGeoApiAvailableErrStr =
    "No Geolocation API available on your device"
