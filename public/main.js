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
        if (data.type === "address")
            localStorage.setItem("ADDRESS_DATA", data.data);
    });
}
const startAppWFlags = (flags) => Elm.Main.init({
    node: document.getElementById("root"),
    flags: flags
});
const freshAppStart = () => Elm.Main.init({
    node: document.getElementById("root")
});
const cachedWeatherData = localStorage.getItem("WEATHER_DATA");
const cachedAddressData = localStorage.getItem("ADDRESS_DATA");
function report(state) {
    console.log(`Permission ${state}`);
}
try {
    if (cachedWeatherData && cachedAddressData) {
        const parsedWeatherData = JSON.parse(cachedWeatherData);
        const parsedAddressData = JSON.parse(cachedAddressData);
        if (navigator.permissions) {
            navigator.permissions.query({ name: "geolocation" }).then((result) => {
                var _a, _b, _c, _d;
                if (result.state === "granted") {
                    main(startAppWFlags({
                        posixTimeNow: Date.now(),
                        cachedWeatherData: parsedWeatherData,
                        country: parsedAddressData.address.country,
                        city: (_a = parsedAddressData.address.city) !== null && _a !== void 0 ? _a : null,
                        state: (_b = parsedAddressData.address.state) !== null && _b !== void 0 ? _b : null,
                        usingGeoLocation: true
                    }));
                }
                else {
                    main(startAppWFlags({
                        posixTimeNow: Date.now(),
                        cachedWeatherData: parsedWeatherData,
                        country: parsedAddressData.address.country,
                        city: (_c = parsedAddressData.address.city) !== null && _c !== void 0 ? _c : null,
                        state: (_d = parsedAddressData.address.state) !== null && _d !== void 0 ? _d : null,
                        usingGeoLocation: false
                    }));
                }
            });
        }
        else {
            main(startAppWFlags({
                posixTimeNow: Date.now(),
                cachedWeatherData: parsedWeatherData,
                country: parsedAddressData.address.country,
                city: (_a = parsedAddressData.address.city) !== null && _a !== void 0 ? _a : null,
                state: (_b = parsedAddressData.address.state) !== null && _b !== void 0 ? _b : null,
                usingGeoLocation: false
            }));
        }
    }
    else if (cachedWeatherData) {
        const parsedData = JSON.parse(cachedWeatherData);
        if (navigator.permissions) {
            navigator.permissions.query({ name: "geolocation" }).then((result) => {
                if (result.state === "granted") {
                    main(startAppWFlags({
                        posixTimeNow: Date.now(),
                        cachedWeatherData: parsedData,
                        usingGeoLocation: true
                    }));
                }
                else {
                    main(startAppWFlags({
                        posixTimeNow: Date.now(),
                        cachedWeatherData: parsedData,
                        usingGeoLocation: false
                    }));
                }
            });
        }
        else {
            main(startAppWFlags({
                posixTimeNow: Date.now(),
                cachedWeatherData: parsedData,
                usingGeoLocation: false
            }));
        }
    }
    else {
        main(freshAppStart());
    }
}
catch (_c) {
    main(freshAppStart());
}
function main(app) {
    app.ports.requestLocation.subscribe(() => {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition((position) => app.ports.locationReceiver.send(position.coords), (error) => app.ports.errorObtainingCurrentPosition.send(error.code));
        }
        else {
            app.ports.noGeoLocationApiAvailableReceiver.send();
        }
    });
}