module Screens.Welcome exposing (..)

import Element exposing (Element, centerX, centerY, column, el, fill, height, none, paddingXY, paragraph, px, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import Ports exposing (..)
import Time exposing (Posix)
import Utils exposing (..)


type alias WelcomeScreenModel =
    { errorMessage : Maybe String
    , manualLocation : ( String, String )
    , manualLocationErr : String

    -- It's only to pass it along to the next step/screen
    , currentTime : Posix

    -- parent will intercept to check this
    , receivedLocation : Maybe { latitude : Float, longitude : Float }
    }


type WelcomeScreenMsg
    = RequestLocationPerms
    | RequestLocationPermsApiError Int
    | NoGeoLocationApi ()
    | ReceivedLocation { latitude : Float, longitude : Float }
      -- manually entering location
    | OnChangeLatitude String
    | OnChangeLongitude String
    | SubmitManualLocationForm


welcomeScreenSubscriptions : WelcomeScreenModel -> Sub WelcomeScreenMsg
welcomeScreenSubscriptions _ =
    Sub.batch
        [ errorObtainingCurrentPosition RequestLocationPermsApiError
        , locationReceiver ReceivedLocation
        , noGeoLocationApiAvailableReceiver NoGeoLocationApi
        ]


welcomeScreenInit : Int -> WelcomeScreenModel
welcomeScreenInit posixTimeNow =
    { errorMessage = Nothing
    , manualLocation = ( "", "" )
    , manualLocationErr = ""
    , receivedLocation = Nothing
    , currentTime = Time.millisToPosix posixTimeNow
    }


welcomeScreenUpdate : WelcomeScreenMsg -> WelcomeScreenModel -> ( WelcomeScreenModel, Cmd WelcomeScreenMsg )
welcomeScreenUpdate msg model =
    let
        ( lat, lon ) =
            model.manualLocation
    in
    case msg of
        RequestLocationPerms ->
            ( { model | errorMessage = Nothing }, requestLocationPerms () )

        ReceivedLocation coords ->
            ( { model | receivedLocation = Just coords }, Cmd.none )

        RequestLocationPermsApiError err ->
            ( { model
                | errorMessage =
                    err
                        |> codeToGeoLocationApiError
                        |> geoLocationApiErrorToString
                        |> Just
              }
            , Cmd.none
            )

        NoGeoLocationApi () ->
            ( { model | errorMessage = Just noGeoApiAvailableErrStr }
            , Cmd.none
            )

        OnChangeLatitude str ->
            ( { model
                | errorMessage = model.errorMessage
                , manualLocation = ( str, lon )
              }
            , Cmd.none
            )

        OnChangeLongitude str ->
            ( { model
                | errorMessage = model.errorMessage
                , manualLocation = ( lat, str )
              }
            , Cmd.none
            )

        SubmitManualLocationForm ->
            case String.toFloat lat of
                Just latFloat ->
                    if latFloat < -90 || latFloat > 90 then
                        ( { model
                            | errorMessage = Nothing
                            , manualLocationErr = "Latitude must be between -90 and 90"
                          }
                        , Cmd.none
                        )

                    else
                        case String.toFloat lon of
                            Just lonFloat ->
                                if lonFloat < -180 || lonFloat > 180 then
                                    ( { model
                                        | errorMessage = Nothing
                                        , manualLocationErr = "Longitude must be between -180 and 180"
                                      }
                                    , Cmd.none
                                    )

                                else
                                    ( { model | receivedLocation = Just { latitude = latFloat, longitude = lonFloat } }, Cmd.none )

                            Nothing ->
                                ( { model
                                    | errorMessage = Nothing
                                    , manualLocationErr = "Longitude must be a valid number"
                                  }
                                , Cmd.none
                                )

                Nothing ->
                    ( { model
                        | errorMessage = Nothing
                        , manualLocationErr = "Latitude must be a valid number"
                      }
                    , Cmd.none
                    )


welcomeScreenView : WelcomeScreenModel -> Element WelcomeScreenMsg
welcomeScreenView { manualLocationErr, manualLocation, errorMessage } =
    let
        ( lat, lon ) =
            manualLocation

        errStr =
            errorMessage |> Maybe.withDefault ""
    in
    el
        [ width fill
        , height fill
        ]
        (el
            [ width fill
            , height fill
            , Background.color primary
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
                        , Background.color black
                        , Font.color primary
                        , Font.bold
                        , paddingXY 24 12
                        , Font.size 22
                        , if errStr == noGeoApiAvailableErrStr then
                            Font.strike

                          else
                            Font.bold
                        ]
                        { label = text "Enable Location Permission", onPress = Just RequestLocationPerms }
                    )
                , el [ Font.bold, centerX, paddingXY 0 15 ] (text "or")
                , column
                    [ centerX
                    , Background.color black
                    ]
                    [ if manualLocationErr /= "" then
                        column [ width fill ]
                            [ paragraph
                                [ paddingXY 24 12
                                , Font.center
                                , spacing 8
                                ]
                                [ el
                                    [ centerX
                                    , Font.color primary
                                    , Font.heavy
                                    , Font.underline
                                    , Font.size 22
                                    ]
                                    (text "Error")
                                , br
                                , el
                                    [ centerX
                                    , Font.color primary
                                    , Font.light
                                    , Font.size 18
                                    ]
                                    (text manualLocationErr)
                                ]
                            , el [ width fill, height (px 1), Background.color primary ] none
                            ]

                      else
                        none
                    , column
                        [ paddingXY 24 24
                        , width fill
                        , spacing 24
                        ]
                        [ Input.text
                            [ width fill
                            , Background.color primary
                            ]
                            { onChange = \l -> OnChangeLatitude l
                            , text = lat
                            , placeholder = Just (Input.placeholder [] (el [ Font.color black ] (text "-150.58")))
                            , label = Input.labelAbove [ Font.color primary ] (text "Latitude:")
                            }
                        , Input.text
                            [ width fill
                            , Background.color primary
                            ]
                            { onChange = \l -> OnChangeLongitude l
                            , text = lon
                            , placeholder = Just (Input.placeholder [] (el [ Font.color black ] (text "75.88")))
                            , label = Input.labelAbove [ Font.color primary ] (text "Longitude:")
                            }
                        ]
                    , el [ width fill, height (px 1), Background.color primary ] none
                    , el [ centerX ]
                        (button
                            [ paddingXY 24 12
                            , centerX
                            , Font.color primary
                            , Font.bold
                            , Font.size 22
                            ]
                            { label = text "Enter coordinates manually", onPress = Just SubmitManualLocationForm }
                        )
                    ]
                ]
            )
        )
