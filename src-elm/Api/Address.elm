module Api.Address exposing (Address, getAddress)

import Http
import Json.Decode exposing (..)
import Utils exposing (Coordinates, nonEmptyString)


reverseGeocodingEndpoint : Coordinates -> String
reverseGeocodingEndpoint { latitude, longitude } =
    "https://nominatim.openstreetmap.org/reverse?lat="
        ++ String.fromFloat latitude
        ++ "&lon="
        ++ String.fromFloat longitude
        ++ "&format=json"
        ++ "&zoom=8"


type alias ReverseGeocodingResponse =
    { address : Address }


reverseGeocodingDecoder : Decoder ReverseGeocodingResponse
reverseGeocodingDecoder =
    map ReverseGeocodingResponse
        (field "address" addressDecoder)


type alias Address =
    { country : String
    , state : Maybe String
    , city : Maybe String
    }


addressDecoder : Decoder Address
addressDecoder =
    map3
        (\country state city ->
            { country = country
            , state = state
            , city = city
            }
        )
        (field "country" string)
        (maybe (field "state" string) |> map (Maybe.andThen nonEmptyString))
        (maybe (field "city" string) |> map (Maybe.andThen nonEmptyString))


getAddress : Coordinates -> (Result Http.Error Address -> msg) -> Cmd msg
getAddress coordinates msg =
    Http.get
        { url = reverseGeocodingEndpoint coordinates
        , expect = Http.expectJson msg (reverseGeocodingDecoder |> map .address)
        }
