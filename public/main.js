"use strict";
var _a, _b;
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
        const data = event.data;
        if (data.type === "meteo")
            localStorage.setItem("WEATHER_DATA", data.data);
        if (data.type === "address") {
            if (data.data == null) {
                localStorage.removeItem("ADDRESS_DATA");
            }
            else {
                localStorage.setItem("ADDRESS_DATA", data.data);
            }
        }
    });
}
const startAppWFlags = (flags) => Elm.Main.init({
    node: document.getElementById("root"),
    flags: flags
});
const freshAppStart = () => Elm.Main.init({
    node: document.getElementById("root"),
    flags: navigator.language || navigator.userLanguage
});
const cachedWeatherData = localStorage.getItem("WEATHER_DATA");
const cachedAddressData = localStorage.getItem("ADDRESS_DATA");
const theme = localStorage.getItem("THEME");
const customThemes = localStorage.getItem("THEMES");
const usingGeo = localStorage.getItem("USING_GEOLOCATION");
let parsedTheme = null;
let parsedCustomThemes = null;
let usingGeoLocation = false;
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
    if (usingGeo) {
        usingGeoLocation = JSON.parse(usingGeo);
    }
    if (cachedWeatherData && cachedAddressData) {
        const parsedWeatherData = JSON.parse(cachedWeatherData);
        const parsedAddressData = JSON.parse(cachedAddressData);
        main(startAppWFlags({
            posixTimeNow: Date.now(),
            cachedWeatherData: parsedWeatherData,
            addressData: {
                country: parsedAddressData.address.country,
                city: (_a = parsedAddressData.address.city) !== null && _a !== void 0 ? _a : null,
                state: (_b = parsedAddressData.address.state) !== null && _b !== void 0 ? _b : null
            },
            usingGeoLocation: usingGeoLocation,
            language: navigator.language || navigator.userLanguage,
            theme: parsedTheme,
            customThemes: parsedCustomThemes
        }));
    }
    else if (cachedWeatherData) {
        const parsedData = JSON.parse(cachedWeatherData);
        main(startAppWFlags({
            posixTimeNow: Date.now(),
            cachedWeatherData: parsedData,
            usingGeoLocation: usingGeoLocation,
            language: navigator.language || navigator.userLanguage,
            theme: parsedTheme,
            customThemes: parsedCustomThemes
        }));
    }
    else {
        main(freshAppStart());
    }
}
catch (_c) {
    main(freshAppStart());
}
function main(app) {
    if (navigator.onLine) {
        app.ports.wentOnline.send(null);
    }
    else {
        app.ports.wentOffline.send(null);
    }
    window.addEventListener("online", () => app.ports.wentOnline.send(null));
    window.addEventListener("offline", () => app.ports.wentOffline.send(null));
    app.ports.checkIfIsOnline.subscribe(() => {
        if (navigator.onLine) {
            app.ports.wentOnline.send(null);
        }
        else {
            app.ports.wentOffline.send(null);
        }
    });
    app.ports.requestLocation.subscribe(() => {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition((position) => {
                localStorage.setItem("USING_GEOLOCATION", "true");
                app.ports.locationReceiver.send(position.coords);
            }, (error) => app.ports.errorObtainingCurrentPosition.send(error.code));
        }
        else {
            app.ports.noGeoLocationApiAvailableReceiver.send(null);
        }
    });
    app.ports.setNotUsingGeoLocation.subscribe(() => {
        localStorage.removeItem("USING_GEOLOCATION");
    });
    app.ports.changedTheme.subscribe((data) => {
        const [pr, pg, pb] = data[0];
        const [sr, sg, sb] = data[1];
        localStorage.setItem("THEME", JSON.stringify({
            primary: { r: pr, g: pg, b: pb },
            secondary: { r: sr, g: sg, b: sb }
        }));
    });
    app.ports.saveCustomThemes.subscribe((data) => {
        localStorage.setItem("THEMES", JSON.stringify(data));
    });
}
