/// <reference path="main.d.ts" />

if ("serviceWorker" in navigator) {
  navigator.serviceWorker
    .register("worker.js")
    .then((e) => {
      console.log("Service Worker Registered");
    })
    .catch((err) => {
      console.log("Error registering: Not on HTTPS");
    });

  navigator.serviceWorker.addEventListener("message", (event) => {
    // NOTE: instead of adding it to cache on SW
    // i'm passing it to here and storing it in localStorage
    // as It's about 9-10~ times faster than using cache.match
    const data = event.data;
    if (data.type === "meteo")
      localStorage.setItem(localStorageKeys.WEATHER_DATA, data.data);
    if (data.type === "address")
      localStorage.setItem(localStorageKeys.ADDRESS_DATA, data.data);
  });
}

const startAppWFlags = (flags: ElmFlags) =>
  Elm.Main.init({
    node: document.getElementById("root"),
    flags: flags
  });

const startApp = () =>
  Elm.Main.init({
    node: document.getElementById("root")
  });

let app: ElmApp;

const cachedWeatherData = localStorage.getItem(localStorageKeys.WEATHER_DATA);
const cachedAddressData = localStorage.getItem(localStorageKeys.ADDRESS_DATA);

try {
  if (cachedWeatherData && cachedAddressData) {
    const parsedWeatherData = JSON.parse(cachedWeatherData);
    const parsedAddressData = JSON.parse(cachedAddressData);
    app = startAppWFlags({
      posixTimeNow: Date.now(),
      cachedWeatherData: parsedWeatherData,
      country: parsedAddressData.address.country,
      state: parsedAddressData.address.state
    });
  } else if (cachedWeatherData) {
    const parsedData = JSON.parse(cachedWeatherData);
    app = startAppWFlags({
      posixTimeNow: Date.now(),
      cachedWeatherData: parsedData
    });
  } else {
    app = startApp();
  }
} catch {
  // NOTE: this is in case there's
  // an error on JSON.parse or accessing parsed data
  // i.e: undefined.country
  app = startApp();
}

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
