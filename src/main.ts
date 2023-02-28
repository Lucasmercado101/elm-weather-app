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
    flags: (navigator.language ||
      (navigator as any).userLanguage) satisfies ElmFlags
  });

const cachedWeatherData = localStorage.getItem(localStorageKeys.WEATHER_DATA);
const cachedAddressData = localStorage.getItem(localStorageKeys.ADDRESS_DATA);
const theme = localStorage.getItem(localStorageKeys.THEME);
const customThemes = localStorage.getItem(localStorageKeys.THEMES);
const usingGeo = localStorage.getItem(localStorageKeys.USING_GEOLOCATION);
const lang = localStorage.getItem(localStorageKeys.LANGUAGE);

let parsedTheme: any = null;
let usingGeoLocation: boolean = false;
let language: number = appLanguage.ENGLISH;
const timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;

try {
  if (theme) {
    parsedTheme = JSON.parse(theme);
    const red = parsedTheme.primary.r * 255;
    const blue = parsedTheme.primary.g * 255;
    const green = parsedTheme.primary.b * 255;
    document.body.style.background = `rgb(${red}, ${blue}, ${green})`;
  } else {
    // default primary
    document.body.style.background = `rgb(254, 225, 66)`;
  }

  if (usingGeo) {
    usingGeoLocation = JSON.parse(usingGeo);
  }
  if (lang) {
    language = JSON.parse(lang);
  }

  if (cachedWeatherData && cachedAddressData) {
    const parsedWeatherData = JSON.parse(cachedWeatherData);
    const parsedAddressData = JSON.parse(cachedAddressData);
    main(
      startAppWFlags({
        posixTimeNow: Date.now(),
        cachedWeatherData: parsedWeatherData,
        addressData: {
          country: parsedAddressData.address.country,
          city: parsedAddressData.address.city ?? null,
          state: parsedAddressData.address.state ?? null
        },
        usingGeoLocation: usingGeoLocation,
        language:
          language || navigator.language || (navigator as any).userLanguage,
        theme: parsedTheme,
        customThemes: customThemes,
        timezone: timeZone
      })
    );
  } else if (cachedWeatherData) {
    const parsedData = JSON.parse(cachedWeatherData);
    main(
      startAppWFlags({
        posixTimeNow: Date.now(),
        cachedWeatherData: parsedData,
        usingGeoLocation: usingGeoLocation,
        language:
          language || navigator.language || (navigator as any).userLanguage,
        theme: parsedTheme,
        customThemes: customThemes,
        timezone: timeZone
      })
    );
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
  if (navigator.onLine) {
    app.ports.wentOnline.send(null);
  } else {
    app.ports.wentOffline.send(null);
  }

  window.addEventListener("online", () => app.ports.wentOnline.send(null));
  window.addEventListener("offline", () => app.ports.wentOffline.send(null));

  app.ports.checkIfIsOnline.subscribe(() => {
    if (navigator.onLine) {
      app.ports.wentOnline.send(null);
    } else {
      app.ports.wentOffline.send(null);
    }
  });
  app.ports.requestLocation.subscribe(() => {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        (position) => {
          // only time it asks for perms is when
          // it's toggled on to use geolocation
          localStorage.setItem(localStorageKeys.USING_GEOLOCATION, "true");
          app.ports.locationReceiver.send(position.coords);
        },
        (error) => app.ports.errorObtainingCurrentPosition.send(error.code)
      );
    } else {
      app.ports.noGeoLocationApiAvailableReceiver.send(null);
    }
  });

  app.ports.setNotUsingGeoLocation.subscribe(() => {
    localStorage.removeItem(localStorageKeys.USING_GEOLOCATION);
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
    document.body.style.background = `rgb(${pr * 255}, ${pg * 255}, ${
      pb * 255
    })`;
  });

  app.ports.saveCustomThemes.subscribe((data) => {
    localStorage.setItem(localStorageKeys.THEMES, JSON.stringify(data));
  });

  app.ports.setAppLanguage.subscribe((data) => {
    localStorage.setItem(localStorageKeys.LANGUAGE, JSON.stringify(data));
  });
}
