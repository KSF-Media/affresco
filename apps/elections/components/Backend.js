import Election from 'election';
import { AreaType } from 'election';

const api = makeApiClient();

export const memoizer = new Memoizer();
const memoize = memoizer.memoize;

export const getArea = memoize(function getArea(identifier) {
  return api.areasIdentifierGet(identifier);
});

export const getCountry = memoize(function getCountry() {
  return api.areasGet({
    "type": [ Election.AreaType.COUNTRY ]
  }).then(({ areas }) => areas[0]);
});

export const getElectoralDistricts = memoize(function getElectoralDistricts() {
  return api.areasGet({
    "type": [ Election.AreaType.ELECTORAL_DISTRICT ]
  });
});

export const getMunicipalities = memoize(function getMunicipalities(electoralDistrict) {
  return api.areasGet({
    "type": [ Election.AreaType.MUNICIPALITY ],
    "parent": electoralDistrict ? [ electoralDistrict ] : null
  });
});

export const getPollingDistricts = memoize(function getPollingDistricts(municipality) {
  return api.areasGet({
    "type": [ Election.AreaType.POLLING_DISTRICT ],
    "parent": municipality ? [ municipality ] : null
  });
});

function makeApiClient() {
  Election.ApiClient.instance.basePath = process.env.ELECTION_BACKEND_URL;
  console.info("Loaded API client configuration", Election.ApiClient.instance)

  var api = new Election.DefaultApi(Election.ApiClient.instance);
  console.info("Created API client instance", api);

  return api;
}

function Memoizer() {
  this.cache = {};
  this.clear = function() {
    this.cache = {};
  }
  let that = this; // oh no
  this.memoize = function(method){
    console.log("memoize", method.name);
    return function() {
      that.cache[method.name] = that.cache[method.name] || {};
      let args = JSON.stringify(arguments);
      that.cache[method.name][args] =
        that.cache[method.name][args] || method.apply(this, arguments);
      return that.cache[method.name][args];
    }
  };
  console.log("Memoizer", this);
}
