const enum localStorageKeys {
  WEATHER_DATA = "WEATHER_DATA",
  ADDRESS_DATA = "ADDRESS_DATA",
  THEME = "THEME"
}

// -----------------
// ELM
// NOTE: 1 to 1 mapping to elm flags
type LanguageFlag = {
  language: string;
};

type CachedWeatherDataFlag = {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: boolean;
  language: string;
};

interface CachedWeatherAndAddressDataFlag {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: boolean;
  country: string;
  state?: string;
  city?: string;
  language: string;
}

type ElmFlags =
  | LanguageFlag
  | CachedWeatherDataFlag
  | CachedWeatherAndAddressDataFlag;
// -----------------

type DataSenderPort<T> = { send: (data: T) => void };

interface ElmApp {
  ports: {
    requestLocation: { subscribe(cb: () => void) };
    changedTheme: {
      subscribe(
        cb: (
          primaryAndSecondary: [
            [number, number, number],
            [number, number, number]
          ]
        ) => void
      );
    };
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
