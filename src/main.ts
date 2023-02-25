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
    flags: navigator.language || (navigator as any).userLanguage
  });

const cachedWeatherData = localStorage.getItem(localStorageKeys.WEATHER_DATA);
const cachedAddressData = localStorage.getItem(localStorageKeys.ADDRESS_DATA);
const theme = localStorage.getItem(localStorageKeys.THEME);
const customThemes = localStorage.getItem(localStorageKeys.THEMES);

let parsedTheme: any = null;
let parsedCustomThemes: any = null;

try {
  if (theme) {
    parsedTheme = JSON.parse(theme);
    const red = parsedTheme.primary.r * 255;
    const blue = parsedTheme.primary.g * 255;
    const green = parsedTheme.primary.b * 255;
    document.body.style.background = `rgb(${red}, ${blue}, ${green})`;
  }

  if (customThemes) {
    parsedCustomThemes = JSON.parse(customThemes);
  }

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
              addressData: {
                country: parsedAddressData.address.country,
                city: parsedAddressData.address.city ?? null,
                state: parsedAddressData.address.state ?? null
              },
              usingGeoLocation: true,
              language: navigator.language || (navigator as any).userLanguage,
              theme: parsedTheme,
              customThemes: parsedCustomThemes
            })
          );
        } else {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedWeatherData,
              addressData: {
                country: parsedAddressData.address.country,
                city: parsedAddressData.address.city ?? null,
                state: parsedAddressData.address.state ?? null
              },
              usingGeoLocation: false,
              language: navigator.language || (navigator as any).userLanguage,
              theme: parsedTheme,
              customThemes: parsedCustomThemes
            })
          );
        }
      });
    } else {
      main(
        startAppWFlags({
          posixTimeNow: Date.now(),
          cachedWeatherData: parsedWeatherData,
          addressData: {
            country: parsedAddressData.address.country,
            city: parsedAddressData.address.city ?? null,
            state: parsedAddressData.address.state ?? null
          },
          // NOTE: I'm asumming here
          usingGeoLocation: false,
          language: navigator.language || (navigator as any).userLanguage,
          theme: parsedTheme,
          customThemes: parsedCustomThemes
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
              language: navigator.language || (navigator as any).userLanguage,
              theme: parsedTheme,
              customThemes: parsedCustomThemes
            })
          );
        } else {
          main(
            startAppWFlags({
              posixTimeNow: Date.now(),
              cachedWeatherData: parsedData,
              usingGeoLocation: false,
              language: navigator.language || (navigator as any).userLanguage,
              theme: parsedTheme,
              customThemes: parsedCustomThemes
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
          language: navigator.language || (navigator as any).userLanguage,
          theme: parsedTheme,
          customThemes: parsedCustomThemes
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

  app.ports.saveCustomTheme.subscribe((data) => {
    const [pr, pg, pb] = data[0];
    const [sr, sg, sb] = data[1];
    const currentCustomThemes = localStorage.getItem(localStorageKeys.THEMES);
    let parsedCustomThemes: [
      [number, number, number],
      [number, number, number]
    ][] = [];

    try {
      if (currentCustomThemes) {
        parsedCustomThemes = JSON.parse(currentCustomThemes);
      }
      if (parsedCustomThemes.length > 10) {
        parsedCustomThemes.shift();
      }

      if (
        parsedCustomThemes.some(
          (theme) =>
            theme[0][0] === pr &&
            theme[0][1] === pg &&
            theme[0][2] === pb &&
            theme[1][0] === sr &&
            theme[1][1] === sg &&
            theme[1][2] === sb
        )
      ) {
        return;
      }
      parsedCustomThemes.push([
        [pr, pg, pb],
        [sr, sg, sb]
      ]);

      localStorage.setItem(
        localStorageKeys.THEMES,
        JSON.stringify(parsedCustomThemes)
      );
    } catch {
      // Do nothing if it fails, it doesn't matter
    }
  });
}
