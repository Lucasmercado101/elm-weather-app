const cacheName = "weatherMate-v1";
const contentToCache = [
  "/public/index.html",
  "/public/main.js",
  "/public/elm.js"
];

self.addEventListener("activate", (event) => {
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
        const client = await clients.get(e.clientId);
        if (url.hostname.includes("meteo")) {
          client.postMessage({ type: "meteo", data: text });
        }

        return response;
      }
      const r = await caches.match(e.request);
      if (r) {
        return r;
      }
      const response = await fetch(e.request);
      const cache = await caches.open(cacheName);
      cache.put(e.request, response.clone()).catch((e) => {});
      return response;
    })()
  );
});

self.addEventListener("install", (e) => {
  e.waitUntil(
    (async () => {
      const cache = await caches.open(cacheName);
      await cache.addAll(contentToCache);
    })()
  );
});
