port module Ports exposing (errorObtainingCurrentPosition, locationReceiver, noGeoLocationApiAvailableReceiver, requestLocPerms)


port requestLocationPerms : () -> Cmd nothing


{-| convenience fn
-}
requestLocPerms : Cmd nothing
requestLocPerms =
    requestLocationPerms ()


port errorObtainingCurrentPosition : (Int -> msg) -> Sub msg


port noGeoLocationApiAvailableReceiver : (() -> msg) -> Sub msg


port locationReceiver : ({ latitude : Float, longitude : Float } -> msg) -> Sub msg
