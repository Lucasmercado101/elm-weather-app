module Localizations exposing (..)

import Element exposing (Element, none, text)
import Http
import Time exposing (Month(..), Weekday(..))
import Utils exposing (GeoLocationApiError(..), dayToString, monthToString)


type Language
    = English
    | Spanish


dailySummary : Language -> String
dailySummary language =
    case language of
        English ->
            "Daily Summary"

        Spanish ->
            "Resumen Diario"


nowItFeels : Language -> Maybe Float -> Maybe Float -> Element msg
nowItFeels language apparentTemp actualTemp =
    case ( apparentTemp, actualTemp ) of
        ( Just apparent, Just actual ) ->
            case language of
                English ->
                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°, it's actually " ++ (actual |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora parece que hace " ++ (apparent |> String.fromFloat) ++ "°, pero en realidad hace " ++ (actual |> String.fromFloat) ++ "°")

        ( Just apparent, Nothing ) ->
            case language of
                English ->
                    text ("Now it feels like " ++ (apparent |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora se siente como" ++ (apparent |> String.fromFloat) ++ "°")

        ( Nothing, Just actual ) ->
            case language of
                English ->
                    text ("Now it's " ++ (actual |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Ahora hace " ++ (actual |> String.fromFloat) ++ "°")

        ( Nothing, Nothing ) ->
            -- NOTE: in theory should never happen
            none


temperatureRange : Language -> Maybe Float -> Maybe Float -> Element msg
temperatureRange language lowestTemp highestTemp =
    case ( lowestTemp, highestTemp ) of
        ( Just lowest, Just highest ) ->
            case language of
                English ->
                    text ("Today, the temperature is felt in the range from " ++ (lowest |> String.fromFloat) ++ "° to " ++ (highest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura se siente en el rango de " ++ (lowest |> String.fromFloat) ++ "° a " ++ (highest |> String.fromFloat) ++ "°")

        ( Just lowest, Nothing ) ->
            case language of
                English ->
                    text ("Today, the temperature lowest temperature is " ++ (lowest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura más baja es " ++ (lowest |> String.fromFloat) ++ "°")

        ( Nothing, Just highest ) ->
            case language of
                English ->
                    text ("Today, the temperature highest temperature is " ++ (highest |> String.fromFloat) ++ "°")

                Spanish ->
                    text ("Hoy, la temperatura más alta es " ++ (highest |> String.fromFloat) ++ "°")

        ( Nothing, Nothing ) ->
            none


wind : Language -> String
wind language =
    case language of
        English ->
            "Wind"

        Spanish ->
            "Viento"


humidity : Language -> String
humidity language =
    case language of
        English ->
            "Humidity"

        Spanish ->
            "Humedad"


visibility : Language -> String
visibility language =
    case language of
        English ->
            "Visibility"

        Spanish ->
            "Visibilidad"


weeklyForecast : Language -> String
weeklyForecast language =
    case language of
        English ->
            "Weekly Forecast"

        Spanish ->
            "Pronóstico Semanal"


dayAndMonth : Language -> ( Time.Weekday, Time.Month ) -> ( String, String )
dayAndMonth language ( day, month ) =
    case language of
        English ->
            ( dayToString day, monthToString month )

        Spanish ->
            ( esDayToString day, esMonthToString month )


esDayToString : Time.Weekday -> String
esDayToString day =
    case day of
        Mon ->
            "Lunes"

        Tue ->
            "Martes"

        Wed ->
            "Miércoles"

        Thu ->
            "Jueves"

        Fri ->
            "Viernes"

        Sat ->
            "Sábado"

        Sun ->
            "Domingo"


esMonthToString : Time.Month -> String
esMonthToString month =
    case month of
        Jan ->
            "Enero"

        Feb ->
            "Febrero"

        Mar ->
            "Marzo"

        Apr ->
            "Abril"

        May ->
            "Mayo"

        Jun ->
            "Junio"

        Jul ->
            "Julio"

        Aug ->
            "Agosto"

        Sep ->
            "Septiembre"

        Oct ->
            "Octubre"

        Nov ->
            "Noviembre"

        Dec ->
            "Diciembre"


attribution : Language -> String
attribution language =
    case language of
        English ->
            "Weather data by"

        Spanish ->
            "Datos meteorológicos por"


attributionAddress : Language -> String
attributionAddress language =
    case language of
        English ->
            "Address data by"

        Spanish ->
            "Datos de dirección por"


geolocation : Language -> String
geolocation language =
    case language of
        English ->
            "Geolocation"

        Spanish ->
            "Geolocalización"


coordinates : Language -> String
coordinates language =
    case language of
        English ->
            "Coordinates"

        Spanish ->
            "Coordenadas"


theme : Language -> String
theme language =
    case language of
        English ->
            "Theme"

        Spanish ->
            "Tema"


languagePicker : Language -> String
languagePicker lang =
    case lang of
        English ->
            "Language"

        Spanish ->
            "Idioma"


cancel : Language -> String
cancel lang =
    case lang of
        English ->
            "cancel"

        Spanish ->
            "cancelar"


confirm : Language -> String
confirm lang =
    case lang of
        English ->
            "Confirm"

        Spanish ->
            "Confirmar"


latitude : Language -> String
latitude lang =
    case lang of
        English ->
            "Latitude"

        Spanish ->
            "Latitud"


longitude : Language -> String
longitude lang =
    case lang of
        English ->
            "Longitude"

        Spanish ->
            "Longitud"


type LatAndLongManualError
    = InvalidLatitude
    | InvalidLongitude
    | OutOfRangeLatitude
    | OutOfRangeLongitude


manualLatitudeAndLongitudeError : Language -> LatAndLongManualError -> String
manualLatitudeAndLongitudeError lang errType =
    case lang of
        English ->
            case errType of
                InvalidLatitude ->
                    "Latitude must be a valid number"

                InvalidLongitude ->
                    "Longitude must be a valid number"

                OutOfRangeLatitude ->
                    "Latitude must be between -90 and 90"

                OutOfRangeLongitude ->
                    "Longitude must be between -180 and 180"

        Spanish ->
            case errType of
                InvalidLatitude ->
                    "Latitud debe ser un número válido"

                InvalidLongitude ->
                    "Longitud debe ser un número válido"

                OutOfRangeLatitude ->
                    "Latitud debe estar entre -90 y 90"

                OutOfRangeLongitude ->
                    "Longitud debe estar entre -180 y 180"


edit : Language -> String
edit lang =
    case lang of
        English ->
            "edit"

        Spanish ->
            "editar"


apply : Language -> String
apply lang =
    case lang of
        English ->
            "apply"

        Spanish ->
            "aplicar"


primaryColor : Language -> String
primaryColor lang =
    case lang of
        English ->
            "Primary Color"

        Spanish ->
            "Color Primario"


secondaryColor : Language -> String
secondaryColor lang =
    case lang of
        English ->
            "Secondary Color"

        Spanish ->
            "Color Secundario"


applied : Language -> String
applied lang =
    case lang of
        English ->
            "applied"

        Spanish ->
            "aplicado"


customThemes : Language -> String
customThemes lang =
    case lang of
        English ->
            "Custom Themes"

        Spanish ->
            "Temas Personalizados"


themes : Language -> String
themes lang =
    case lang of
        English ->
            "Themes"

        Spanish ->
            "Temas"


geoLocationApiError : Language -> GeoLocationApiError -> String
geoLocationApiError lang error =
    case lang of
        English ->
            case error of
                PermissionDenied ->
                    "Permission to access location was denied"

                PositionUnavailable ->
                    "Current location information is unavailable"

                Timeout ->
                    "The request to get user location timed out"

        Spanish ->
            case error of
                PermissionDenied ->
                    "Permiso para acceder a la ubicación denegado"

                PositionUnavailable ->
                    "La información de ubicación actual no está disponible"

                Timeout ->
                    "La solicitud para obtener la ubicación del usuario ha expirado"


noInternet : Language -> String
noInternet lang =
    case lang of
        English ->
            "No internet connection"

        Spanish ->
            "Sin conexión a internet"


deviceDoesNotSupportGeolocation : Language -> String
deviceDoesNotSupportGeolocation lang =
    case lang of
        English ->
            "Your device doesn't support geolocation"

        Spanish ->
            "Su dispositivo no soporta geolocalización"


enterCoordinatesManually : Language -> String
enterCoordinatesManually lang =
    case lang of
        English ->
            "Enter coordinates manually"

        Spanish ->
            "Introduzca las coordenadas manualmente"


enableLocationPermission : Language -> String
enableLocationPermission lang =
    case lang of
        English ->
            "Enable location permission"

        Spanish ->
            "Habilitar permiso de ubicación"


loading : Language -> String
loading lang =
    case lang of
        English ->
            "Loading"

        Spanish ->
            "Cargando"


httpError : Language -> Http.Error -> String
httpError lang err =
    case lang of
        English ->
            case err of
                Http.BadUrl url ->
                    "Bad URL: " ++ url

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Error"

                Http.BadStatus response ->
                    "Bad Status: " ++ String.fromInt response

                Http.BadBody _ ->
                    "Error parsing body"

        Spanish ->
            case err of
                Http.BadUrl url ->
                    "URL incorrecta: " ++ url

                Http.Timeout ->
                    "Tiempo de espera agotado"

                Http.NetworkError ->
                    "Error de red"

                Http.BadStatus response ->
                    "Estado incorrecto: " ++ String.fromInt response

                Http.BadBody _ ->
                    "Error al procesar el cuerpo"
