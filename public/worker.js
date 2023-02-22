const cacheName = "weatherMate-v1";
const contentToCache = ["./index.html", "./main.js", "./elm.js"];

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
        } else {
          client.postMessage({ type: "address", data: text });
        }

        return response;
      }
      const r = await caches.match(e.request);
      // console.log(`[Service Worker] Fetching resource: ${e.request.url}`);
      if (r) {
        return r;
      }
      const response = await fetch(e.request);
      const cache = await caches.open(cacheName);
      // console.log(`[Service Worker] Caching new resource: ${e.request.url}`);
      cache.put(e.request, response.clone()).catch((e) => {
        // console.log("error caching", e);
      });
      return response;
    })()
  );
});

self.addEventListener("install", (e) => {
  // console.log("[Service Worker] Install");
  e.waitUntil(
    (async () => {
      const cache = await caches.open(cacheName);
      // console.log("[Service Worker] Caching all: app shell and content");
      // TODO: this can fail, try catch
      await cache.addAll(contentToCache);
    })()
  );
});
