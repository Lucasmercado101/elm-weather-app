// NOTE: 1 to 1 mapping to elm flags
type CachedWeatherDataFlag = {
  posixTimeNow: number;
  cachedWeatherData: any;
};

interface CachedWeatherAndAddressDataFlag {
  posixTimeNow: number;
  cachedWeatherData: any;
  country: string;
  state: string;
}

type ElmFlags = CachedWeatherDataFlag | CachedWeatherAndAddressDataFlag;
// -----------------

type DataSenderPort<T> = { send: (data: T) => void };

interface ElmApp {
  ports: {
    requestLocationPerms: { subscribe(cb: () => void) };
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
