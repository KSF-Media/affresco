exports.sso = window.JANRAIN ? window.JANRAIN.SSO : null;

exports.loadConfig = function () {
  var baseUrl = window.location.protocol + "//" + window.location.host;
  var config = {
    client_id: process.env.JANRAIN_LOGIN_CLIENT_ID,
    flow_name: "standard",
    flow_version: process.env.JANRAIN_FLOW_VERSION,
    locale: "sv-FI",
    redirect_uri: baseUrl,
    sso_server: process.env.JANRAIN_SSO_SERVER,
    xd_receiver: baseUrl + process.env.JANRAIN_XD_RECEIVER_PATH,
  };
  console.log("SSO config", config);
  return window.JANRAIN ? config : null;
};
