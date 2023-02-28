const enum localStorageKeys {
  WEATHER_DATA = "WEATHER_DATA",
  ADDRESS_DATA = "ADDRESS_DATA",
  THEME = "THEME",
  THEMES = "THEMES",
  USING_GEOLOCATION = "USING_GEOLOCATION",
  LANGUAGE = "LANGUAGE"
}

const enum appLanguage {
  ENGLISH = 1,
  SPANISH = 2
}

type localStorageJSONValue = string | null;

// -----------------
// ELM
// NOTE: 1 to 1 mapping to elm flags
type InitialFlag = string;

type CachedWeatherDataFlag = {
  posixTimeNow: number;
  cachedWeatherData: any;
  usingGeoLocation: localStorageJSONValue;
  language: string | number;
  theme: any;
  customThemes: localStorageJSONValue;
  timezone: string;
};

interface CachedWeatherAndAddressDataFlag {
  posixTimeNow: number;
  cachedWeatherData: any;
  addressData: { country: string; state?: string; city?: string };
  usingGeoLocation: localStorageJSONValue;
  language: string | number;
  theme: any;
  customThemes: localStorageJSONValue;
  timezone: string;
}

type ElmFlags =
  | InitialFlag
  | CachedWeatherDataFlag
  | CachedWeatherAndAddressDataFlag;
// -----------------

// RGB
type colorTuple = [number, number, number];
type themeTuple = [colorTuple, colorTuple];

type SignalSenderPort = { send: (data: null) => void };
type DataSenderPort<T> = { send: (data: T) => void };
type DataReceiverPort<T> = { subscribe(cb: (data: T) => void) };
type SignalReceiverPort = { subscribe(cb: () => void) };

interface ElmApp {
  ports: {
    requestLocation: SignalReceiverPort;
    setNotUsingGeoLocation: SignalReceiverPort;
    changedTheme: DataReceiverPort<themeTuple>;
    saveCustomThemes: DataReceiverPort<themeTuple[]>;
    locationReceiver: DataSenderPort<GeolocationCoordinates>;
    errorObtainingCurrentPosition: DataSenderPort<
      errorObtainingCurrentPosition["code"]
    >;
    noGeoLocationApiAvailableReceiver: SignalSenderPort;
    wentOffline: SignalSenderPort;
    wentOnline: SignalSenderPort;
    checkIfIsOnline: SignalReceiverPort;
    setAppLanguage: DataReceiverPort<number>;
  };
}

interface Elm {
  Main: {
    init: (initFn: { node: HTMLElement | null; flags?: ElmFlags }) => ElmApp;
  };
}
declare const Elm: Elm;
