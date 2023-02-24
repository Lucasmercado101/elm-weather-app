const cacheName = "weatherMate-v1";
const contentToCache = ["./index.html", "./main.js", "./elm.js"];

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
  event.waitUntil(self.registration?.navigationPreload.enable());
});

self.addEventListener("fetch", (e) => {
  const url = new URL(e.request.url);

  e.respondWith(
    (async () => {
      if (
        url.hostname.includes("meteo") ||
        url.hostname.includes("openstreetmap")
      ) {
        const response = await fetch(e.request);
        const clone = response.clone();
        const text = await clone.text();

        await clients
          .get(e.clientId)
          .then((client) => {
            if (url.hostname.includes("meteo")) {
              client.postMessage({ type: "meteo", data: text });
            } else {
              try {
                const parsedData = JSON.parse(text);
                if (
                  !parsedData?.address?.country &&
                  !parsedData?.address?.city &&
                  !parsedData?.address?.state
                ) {
                  client.postMessage({ type: "address", data: null });
                } else {
                  client.postMessage({ type: "address", data: text });
                }
              } catch {}
            }
          })
          .catch((e) => {
            // don't care if it fails
          });

        return response;
      }
      const r = await caches.match(e.request);
      if (r) return r;

      const response = await fetch(e.request);
      const cache = await caches.open(cacheName);

      cache.put(e.request, response.clone()).catch((e) => {
        // don't care if it fails
      });
      return response;
    })()
  );
});

self.addEventListener("install", (e) => {
  e.waitUntil(
    (async () => {
      const cache = await caches.open(cacheName);
      await cache.addAll(contentToCache).catch((e) => {
        // don't care if it fails
      });
    })()
  );
});
