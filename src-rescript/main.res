open GeoLocationApi
type navigator = {"geolocation": option<geoLocation>}
@val external navigator: navigator = "navigator"

// if ("serviceWorker" in navigator) {
//   navigator.serviceWorker
//     .register("worker.js")
//     .then((e) => {
//       console.log("Service Worker Registered");
//     })
//     .catch((err) => {
//       console.log("Error registering: Not on HTTPS");
//     });

//   navigator.serviceWorker.ready.then((registration) => {
//     registration.active.postMessage("Hi service worker");
//   });

//   navigator.serviceWorker.addEventListener("message", (event) => {
//     // NOTE: instead of adding it to cache on SW
//     // i'm passing it to here and storing it in localStorage
//     // as It's about 9-10~ times faster than using cache.match
//     const data = event.data;
//     if (data.type === "meteo") localStorage.setItem("weatherData", data.data);
//     if (data.type === "address") localStorage.setItem("address", data.data);
//   });
// }

external localStorage: {
  "getItem": (. string) => Js.nullable<string>,
  "setItem": (. string, string) => unit,
  "removeItem": (. string) => unit,
  "key": (. int) => Js.nullable<string>,
  "clear": (. unit) => unit,
} = "localStorage"

@scope("JSON") @val
external parse: string => 'a = "parse"

@scope("document") @val
external getElementById: (. string) => Dom.element = "getElementById"

type elmInit<'a> = {
  node: Dom.element,
  flags: 'a,
}

// NOTE: 1 to 1 mapping to elm flags and ports
// ports
type subscriptionPort<'incoming> = {subscribe: (. 'incoming => unit) => unit}

type messageSenderPort<'a> = {send: (. 'a) => unit}

type elmPorts = {
  requestLocationPerms: subscriptionPort<unit>,
  errorObtainingCurrentPosition: messageSenderPort<int>,
  noGeoLocationApiAvailableReceiver: messageSenderPort<unit>,
  locationReceiver: messageSenderPort<{"latitude": float, "longitude": float}>,
}

type elmApp = {ports: elmPorts}

// flags
type cachedWeatherDataFlag<'a> = {posixTimeNow: float, cachedWeatherData: 'a}
type cachedWeatherAndAddressDataFlag<'a> = {
  posixTimeNow: float,
  cachedWeatherData: 'a,
  country: string,
  state: string,
}

@scope(("Elm", "Main")) @val
external elmInitWithCachedWeatherAndAddressDataFlag: elmInit<
  cachedWeatherAndAddressDataFlag<'a>,
> => elmApp = "init"

@scope(("Elm", "Main")) @val
external elmInitWithCachedWeatherDataFlag: elmInit<cachedWeatherDataFlag<'a>> => elmApp = "init"

@scope(("Elm", "Main")) @val
external elmInitNoFlags: elmInit<unit> => elmApp = "init"

// ------------------------------

let rootElement: Dom.element = getElementById(. "root")

let cachedWeatherData = localStorage["getItem"](. "weatherData")
let cachedAddressData = localStorage["getItem"](. "address")

let main = (app: elmApp) => {
  app.ports.requestLocationPerms.subscribe(.() => {
    switch navigator["geolocation"] {
    | Some(geoLocation) =>
      geoLocation->getCurrentPosition2(
        (. position) => {
          app.ports.locationReceiver.send(. {
            "latitude": position.coords.latitude,
            "longitude": position.coords.longitude,
          })
        },
        (. error) => {
          app.ports.errorObtainingCurrentPosition.send(. error.code)
        },
      )

    | None => app.ports.noGeoLocationApiAvailableReceiver.send(. ())
    }
  })
}

try {
  switch (Js.Nullable.toOption(cachedWeatherData), Js.Nullable.toOption(cachedAddressData)) {
  | (Some(cachedWeatherData), Some(cachedAddressData)) => {
      let parsedWeatherData = parse(cachedWeatherData)
      let parsedAddressData = parse(cachedAddressData)

      elmInitWithCachedWeatherAndAddressDataFlag({
        node: rootElement,
        flags: {
          posixTimeNow: Js.Date.now(),
          cachedWeatherData: parsedWeatherData,
          country: parsedAddressData["address"]["country"],
          state: parsedAddressData["address"]["state"],
        },
      })
    }

  | (Some(cachedWeatherData), None) => {
      let parsedWeatherData = parse(cachedWeatherData)
      elmInitWithCachedWeatherDataFlag({
        node: rootElement,
        flags: {
          posixTimeNow: Js.Date.now(),
          cachedWeatherData: parsedWeatherData,
        },
      })
    }

  | (None, _) => elmInitNoFlags({node: rootElement, flags: ()})
  }
} catch {
| _ => elmInitNoFlags({node: rootElement, flags: ()})
}->main
