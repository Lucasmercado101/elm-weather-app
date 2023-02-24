port module Ports exposing (changedTheme, errorObtainingCurrentPosition, locationReceiver, noGeoLocationApiAvailableReceiver, requestLoc)


port requestLocation : () -> Cmd nothing


{-| convenience fn
-}
requestLoc : Cmd nothing
requestLoc =
    requestLocation ()


port errorObtainingCurrentPosition : (Int -> msg) -> Sub msg


port noGeoLocationApiAvailableReceiver : (() -> msg) -> Sub msg


port locationReceiver : ({ latitude : Float, longitude : Float } -> msg) -> Sub msg


type alias RGB =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )


port changedTheme : RGB -> Cmd msg
