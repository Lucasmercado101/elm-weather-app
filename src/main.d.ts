const enum localStorageKeys {
  WEATHER_DATA = "WEATHER_DATA",
  ADDRESS_DATA = "ADDRESS_DATA",
  THEME = "THEME",
  THEMES = "THEMES"
}

// -----------------
// ELM
// NOTE: 1 to 1 mapping to elm flags
type LanguageFlag = string;

type CachedWeatherDataFlag = {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: boolean;
  language: string;
  theme: any;
  customThemes: any;
};

interface CachedWeatherAndAddressDataFlag {
  posixTimeNow: number;
  cachedWeatherData: any;
  addressData: { country: string; state?: string; city?: string };
  usingGeoLocation: boolean;
  language: string;
  theme: any;
  customThemes: any;
}

type ElmFlags =
  | LanguageFlag
  | CachedWeatherDataFlag
  | CachedWeatherAndAddressDataFlag;
// -----------------

// RGB
type colorTuple = [number, number, number];
type themeTuple = [colorTuple, colorTuple];

type DataSenderPort<T> = { send: (data: T) => void };
type DataReceiverPort<T> = { subscribe(cb: (data: T) => void) };

interface ElmApp {
  ports: {
    requestLocation: { subscribe(cb: () => void) };
    changedTheme: DataReceiverPort<themeTuple>;
    saveCustomThemes: DataReceiverPort<themeTuple[]>;
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
