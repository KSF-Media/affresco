import Election from 'election';

const api = makeApiClient();

export function getArea(identifier) {
  return api.areasIdentifierGet(identifier);
}

export function getElectoralDistricts() {
  return api.areasGet({ "type": ["ELECTORAL_DISTRICT"] });
}

export function getMunicipalities(electoralDistrict) {
  return api.areasGet({
    "type": [ "MUNICIPALITY" ],
    "parent": electoralDistrict ? [ electoralDistrict ] : null
  });
}

export function getPollingDistricts(municipality) {
  return api.areasGet({
    "type": [ "POLLING_DISTRICT" ],
    "parent": municipality ? [ municipality ] : null
  });
}

function makeApiClient() {
  Election.ApiClient.instance.basePath = process.env.ELECTION_BACKEND_URL;
  console.info("Loaded API client configuration", Election.ApiClient.instance)

  var api = new Election.DefaultApi(Election.ApiClient.instance);
  console.info("Created API client instance", api);

  return api;
}
