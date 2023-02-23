const enum localStorageKeys {
  WEATHER_DATA = "WEATHER_DATA",
  ADDRESS_DATA = "ADDRESS_DATA"
}

// -----------------
// ELM
// NOTE: 1 to 1 mapping to elm flags
type CachedWeatherDataFlag = {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: boolean;
};

interface CachedWeatherAndAddressDataFlag {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: boolean;
  country: string;
  state: string;
}

type ElmFlags = CachedWeatherDataFlag | CachedWeatherAndAddressDataFlag;
// -----------------

type DataSenderPort<T> = { send: (data: T) => void };

interface ElmApp {
  ports: {
    requestLocation: { subscribe(cb: () => void) };
    locationReceiver: DataSenderPort<GeolocationCoordinates>;
    errorObtainingCurrentPosition: DataSenderPort<
      errorObtainingCurrentPosition["code"]
    >;
    noGeoLocationApiAvailableReceiver: { send: () => void };
  };
}

interface Elm {
  Main: {
    init: (initFn: { node: HTMLElement | null; flags?: ElmFlags }) => ElmApp;
  };
}
declare const Elm: Elm;
