import Election from 'election';

const api = makeApiClient();

export function getArea(identifier) {
  return api.areasIdentifierGet(identifier);
}

function makeApiClient() {
  Election.ApiClient.instance.basePath = process.env.ELECTION_BACKEND_URL;
  console.info("Loaded API client configuration", Election.ApiClient.instance)

  var api = new Election.DefaultApi(Election.ApiClient.instance);
  console.info("Created API client instance", api);

  return api;
}
