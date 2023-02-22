type geoLocation
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
// PERMISSION_DENIED = 1;
// POSITION_UNAVAILABLE = 2;
// TIMEOUT = 3;

type geoLocationPositionError = {
  code: int,
  message: string,
}

// https://w3c.github.io/geolocation-api/#geolocation_interface
type positionCallback = (. geoLocationPosition) => unit
type positionErrorCallback = (. geoLocationPositionError) => unit

type watchId = int

@send external getCurrentPosition: (geoLocation, positionCallback) => unit = "getCurrentPosition"
@send
external getCurrentPosition2: (geoLocation, positionCallback, positionErrorCallback) => unit =
  "getCurrentPosition"

@send
external getCurrentPosition3: (
  geoLocation,
  positionCallback,
  positionErrorCallback,
  positionOptions,
) => unit = "getCurrentPosition"

@send external watchPosition: (geoLocation, positionCallback) => watchId = "watchPosition"
@send external watchPosition2: (geoLocation, positionCallback) => watchId = "watchPosition"
@send
external watchPosition3: (geoLocation, positionCallback, positionOptions) => watchId =
  "watchPosition"
