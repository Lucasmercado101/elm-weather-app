port module Ports exposing (..)


port requestLocationPerms : () -> Cmd nothing


port errorObtainingCurrentPosition : (Int -> msg) -> Sub msg


port noGeoLocationApiAvailableReceiver : (() -> msg) -> Sub msg


port locationReceiver : ({ latitude : Float, longitude : Float } -> msg) -> Sub msg

