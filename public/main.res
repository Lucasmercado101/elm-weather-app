type localStorage = LocalStorage 

external localStorage: {
  "getItem": (. string) => Js.nullable<string>,
  "setItem": (. string, string) => unit,
  "removeItem": (. string) => unit,
  "key": (. int) => Js.nullable<string>,
  "clear": (. unit) => unit,
} = "localStorage"

@scope("JSON") @val
external parse: string => 'a = "parse"



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

type cachedWeatherDataFlag<'a> = {
  posixTimeNow: float,
  cachedWeatherData: 'a
};


type cachedWeatherAndAddressDataFlag<'a> = {
  posixTimeNow: float,
  cachedWeatherData: 'a,
  country : string,
  state: string
};

// NOTE: 1 to 1 mapping to elm flags
let cachedWeatherDataFlag: {"weatherData": 'a} => cachedWeatherDataFlag<'a> = cachedWeatherData => {
  posixTimeNow: Js.Date.now(),
  cachedWeatherData: cachedWeatherData["weatherData"],
}

let cachedWeatherAndAddressDataFlag: {"country": string, "state": string, "weatherData": 'a } => cachedWeatherAndAddressDataFlag<'a> = cachedWeatherData => {
  posixTimeNow: Js.Date.now(),
  cachedWeatherData: cachedWeatherData["weatherData"],
  country : cachedWeatherData["country"],
  state: cachedWeatherData["state"]
}
// ------------------------------


let cachedWeatherData = localStorage["getItem"](. "weatherData")
let cachedAddressData = localStorage["getItem"](. "address");


%%raw(`
const startAppWFlags = (flags) =>
  Elm.Main.init({
    node: document.getElementById("root"),
    flags: flags
  });
`)

try {
  let a = 1
  switch (Js.Nullable.toOption(cachedWeatherData), Js.Nullable.toOption(cachedAddressData)) {
  | (Some(cachedWeatherData), Some(cachedAddressData)) => {
      let parsedWeatherData = parse(cachedWeatherData)
      let parsedAddressData = parse(cachedAddressData)
    }

  | (Some(cachedWeatherData), None) => ()
  | (None, Some(cachedAddressData)) => ()
  | (None, None) => ()
  }
} catch {
| _ => ()
}


%%raw(`
try {
  if (cachedWeatherData && cachedAddressData) {
    const parsedWeatherData = JSON.parse(cachedWeatherData);
    const parsedAddressData = JSON.parse(cachedAddressData);
    main(
      startAppWFlags(
        cachedWeatherAndAddressDataFlag({
          weatherData: parsedWeatherData,
          country: parsedAddressData.address.country,
          state: parsedAddressData.address.state
        })
      )
    );
  } else if (cachedWeatherData) {
    const parsedData = JSON.parse(cachedWeatherData);
    main(
      startAppWFlags(cachedWeatherDataFlag({ cachedWeatherData: parsedData }))
    );
  } else {
    main(startAppWFlags());
  }
} catch {
  // NOTE: this is in case there's
  // an error on JSON.parse or accessing parsed data
  // i.e: undefined.country
  main(startAppWFlags());
}

function main(app) {
  app.ports.requestLocationPerms.subscribe(() => {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        (position) => app.ports.locationReceiver.send(position.coords),
        (error) => app.ports.errorObtainingCurrentPosition.send(error.code)
      );
    } else {
      app.ports.noGeoLocationApiAvailableReceiver.send();
    }
  });
}
`)