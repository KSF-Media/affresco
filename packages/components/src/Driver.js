// Would require something else too if we used web workers
export const webEnvironment = typeof window !== "undefined";
var driver = null;

export function globalDriver() {
  return driver;
}

export function setDriver(d) {
  return function () {
    driver = d;
  };
}
