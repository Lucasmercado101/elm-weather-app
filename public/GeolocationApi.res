// translated from https://w3c.github.io/geolocation-api/#geolocation_interface

type positionOptions = {
  enableHighAccuracy: bool,
  timeout: float,
  maximumAge: float,
}

// https://w3c.github.io/geolocation-api/#dom-geolocationcoordinates
type geoLocationCoordinates = {
  accuracy: float,
  latitude: float,
  longitude: float,
  altitude?: float,
  altitudeAccuracy?: float,
  heading?: float,
  speed?: float,
}

// https://w3c.github.io/geolocation-api/#dom-geolocationposition
type geoLocationPosition = {
  coords: geoLocationCoordinates,
  timestamp: float,
}

// https://w3c.github.io/geolocation-api/#dom-geolocationpositionerror
type permissionDenied = [#1]
type positionUnavailable = [#2]
type timeout = [#3]

type code = [
  | permissionDenied
  | positionUnavailable
  | timeout
]

type geoLocationPositionError = {
  code: code,
  message: string,
}

// https://w3c.github.io/geolocation-api/#geolocation_interface
type positionCallback = (. geoLocationPosition) => unit
type positionErrorCallback = (. geoLocationPositionError) => unit

type getCurrentPosition = (positionCallback, positionErrorCallback, positionOptions) => unit

type watchId = int
type watchPosition = (positionCallback, positionErrorCallback, positionOptions) => watchId

type geoLocation = {
  getCurrentPosition: (positionErrorCallback, positionErrorCallback, positionOptions) => unit,
}
