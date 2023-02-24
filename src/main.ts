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
    if (data.type === "address") {
      if (data.data == null) {
        localStorage.removeItem(localStorageKeys.ADDRESS_DATA);
      } else {
        localStorage.setItem(localStorageKeys.ADDRESS_DATA, data.data);
      }
    }
  });
}

const startAppWFlags = (flags: ElmFlags) =>
  Elm.Main.init({
    node: document.getElementById("root"),
    flags: flags
  });

const freshAppStart = () =>
  Elm.Main.init({
    node: document.getElementById("root"),
    flags: {
      language: navigator.language || (navigator as any).userLanguage
    }
  });

const cachedWeatherData = localStorage.getItem(localStorageKeys.WEATHER_DATA);
const cachedAddressData = localStorage.getItem(localStorageKeys.ADDRESS_DATA);

function report(state: any) {
  console.log(`Permission ${state}`);
}

try {
  if (cachedWeatherData && cachedAddressData) {
    const parsedWeatherData = JSON.parse(cachedWeatherData);
    const parsedAddressData = JSON.parse(cachedAddressData);
    if (navigator.permissions) {
      navigator.permissions.query({ name: "geolocation" }).then((result) => {
        if (result.state === "granted") {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedWeatherData,
              country: parsedAddressData.address.country,
              city: parsedAddressData.address.city ?? null,
              state: parsedAddressData.address.state ?? null,
              usingGeoLocation: true,
              language: navigator.language || (navigator as any).userLanguage
            })
          );
        } else {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedWeatherData,
              country: parsedAddressData.address.country,
              city: parsedAddressData.address.city ?? null,
              state: parsedAddressData.address.state ?? null,
              usingGeoLocation: false,
              language: navigator.language || (navigator as any).userLanguage
            })
          );
        }
      });
    } else {
      main(
        startAppWFlags({
          posixTimeNow: Date.now(),
          cachedWeatherData: parsedWeatherData,
          country: parsedAddressData.address.country,
          city: parsedAddressData.address.city ?? null,
          state: parsedAddressData.address.state ?? null,
          // NOTE: I'm asumming here
          usingGeoLocation: false,
          language: navigator.language || (navigator as any).userLanguage
        })
      );
    }
  } else if (cachedWeatherData) {
    const parsedData = JSON.parse(cachedWeatherData);
    if (navigator.permissions) {
      navigator.permissions.query({ name: "geolocation" }).then((result) => {
        if (result.state === "granted") {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedData,
              usingGeoLocation: true,
              language: navigator.language || (navigator as any).userLanguage
            })
          );
        } else {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedData,
              usingGeoLocation: false,
              language: navigator.language || (navigator as any).userLanguage
            })
          );
        }
      });
    } else {
      main(
        startAppWFlags({
          posixTimeNow: Date.now(),
          cachedWeatherData: parsedData,
          usingGeoLocation: false,
          language: navigator.language || (navigator as any).userLanguage
        })
      );
    }
  } else {
    main(freshAppStart());
  }
} catch {
  // NOTE: this is in case there's
  // an error on JSON.parse or accessing parsed data
  // i.e: undefined.country
  main(freshAppStart());
}
function main(app: ElmApp) {
  app.ports.requestLocation.subscribe(() => {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        (position) => app.ports.locationReceiver.send(position.coords),
        (error) => app.ports.errorObtainingCurrentPosition.send(error.code)
      );
    } else {
      app.ports.noGeoLocationApiAvailableReceiver.send();
    }
  });
  app.ports.changedTheme.subscribe((data) => {
    const [pr, pg, pb] = data[0];
    const [sr, sg, sb] = data[1];
    localStorage.setItem(
      localStorageKeys.THEME,
      JSON.stringify({
        primary: { r: pr, g: pg, b: pb },
        secondary: { r: sr, g: sg, b: sb }
      })
    );
  });
}
