"use strict";
console.time("time to initial elm app");

if ("serviceWorker" in navigator) {
  navigator.serviceWorker
    .register("worker.js")
    .then((e) => {
      console.log("Service Worker Registered");
    })
    .catch((err) => {
      console.log("Error registering: Not on HTTPS");
    });

  navigator.serviceWorker.ready.then((registration) => {
    // console.log(caches.open().then(e => ));
    registration.active.postMessage("Hi service worker");
  });

  navigator.serviceWorker.addEventListener("message", (event) => {
    // NOTE: instead of adding it to cache on SW
    // i'm passing it to here and storing it in localStorage
    // as It's about 9-10~ times faster than using cache.match
    const data = event.data;
    if (data.type === "meteo") {
      try {
        localStorage.setItem("meteo", data.data);
      } catch (e) {
        console.log("ERROR STORING", e);
      }
    }
  });
}

const startAppWFlags = (flags) =>
  Elm.Main.init({
    node: document.getElementById("root"),
    flags: { posixTimeNow: Date.now(), ...flags }
  });

try {
  const meteoData = localStorage.getItem("meteo");
  if (meteoData) {
    // TODO: try catch here
    const parsedData = JSON.parse(meteoData);
    main(startAppWFlags(parsedData));
  } else {
    // NOTE: I could send a flag to indicate
    // that nothing is cached and should therefore
    // cache the queries as soon as possible
    // but it doesn't seem to be worth it as they
    // already get cached, and performance improves,
    // after the Service Worker gets installed and the data
    // is queried and therefore cached
    if (navigator.permissions) {
      navigator.permissions.query({ name: "geolocation" }).then((result) => {
        if (result.state === "granted") {
          // NOTE:
          // Don't know if this extra IF is necessary
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
              (position) => {
                const app = startAppWFlags({
                  latitude: position.coords.latitude,
                  longitude: position.coords.longitude
                });
                main(app);
              },
              (error) => {
                const app = startAppWFlags();
                // TODO: handle this error in elm logic
                app.ports.errorObtainingCurrentPosition.send(error.code);
                main(app);
              }
            );
          } else {
            // TODO: JD.VALUE in elm logic, to handle these cases
            const app = startAppWFlags();
            main(app);
          }
        } else if (result.state === "prompt") {
          const app = startAppWFlags();
          main(app);
        } else if (result.state === "denied") {
          // TODO:
          const app = startAppWFlags();
          main(app);
        }
        // TODO:
        // result.onchange = () => {
        //   if (result.state === "granted") {
        //     app.ports.locationPermissionGrantedReceiver.send();
        //   } else if (result.state === "prompt") {
        //     app.ports.locationPermissionPromptReceiver.send();
        //   } else if (result.state === "denied") {
        //     app.ports.locationPermissionDeniedReceiver.send();
        //   }
        // };
      });
    } else {
      const app = startAppWFlags();
      main(app);
    }
  }
} catch {}

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
  console.timeEnd("time to initial elm app");
}
