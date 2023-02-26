port module Ports exposing
    ( changedTheme
    , checkIfOnline
    , errorObtainingCurrentPosition
    , locationReceiver
    , noGeoLocationApiAvailableReceiver
    , notUsingGeo
    , requestLoc
    , saveCustomThemes
    , wentOffline
    , wentOnline
    )

import Utils exposing (RGB)


port requestLocation : () -> Cmd nothing


{-| convenience fn
-}
requestLoc : Cmd nothing
requestLoc =
    requestLocation ()


port errorObtainingCurrentPosition : (Int -> msg) -> Sub msg


port noGeoLocationApiAvailableReceiver : (() -> msg) -> Sub msg


port locationReceiver : ({ latitude : Float, longitude : Float } -> msg) -> Sub msg


type alias ThemeColorTuple =
    ( RGB, RGB )


port changedTheme : ThemeColorTuple -> Cmd msg


port saveCustomThemes : List ThemeColorTuple -> Cmd msg


port setNotUsingGeoLocation : () -> Cmd msg


notUsingGeo : Cmd msg
notUsingGeo =
    setNotUsingGeoLocation ()


port wentOnline : (() -> msg) -> Sub msg


port wentOffline : (() -> msg) -> Sub msg


port checkIfIsOnline : () -> Cmd msg


checkIfOnline : Cmd msg
checkIfOnline =
    checkIfIsOnline ()
