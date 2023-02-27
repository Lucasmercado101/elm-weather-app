port module Ports exposing
    ( changedTheme
    , checkIfOnline
    , errorObtainingCurrentPosition
    , langFromInt
    , locationReceiver
    , noGeoLocationApiAvailableReceiver
    , notUsingGeo
    , requestLoc
    , saveCustomThemes
    , userSetLanguage
    , wentOffline
    , wentOnline
    )

import Localizations exposing (Language(..))
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


port setAppLanguage : Int -> Cmd msg


userSetLanguage : Language -> Cmd msg
userSetLanguage lang =
    case lang of
        English ->
            setAppLanguage 2

        Spanish ->
            setAppLanguage 1


langFromInt : Int -> Language
langFromInt i =
    case i of
        1 ->
            English

        2 ->
            Spanish

        _ ->
            English
