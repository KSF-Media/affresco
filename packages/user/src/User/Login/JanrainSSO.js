export const sso = typeof window !== "undefined" && window.JANRAIN ? window.JANRAIN.SSO : null;

export function loadConfig() {
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
  /* In order for the failure callback to work, the config object needs to have
   * a field called nologin_callback with the value "nologin_callback", but for
   * some reason (bug in Janrain's SSO code) we can't set it through the object
   * we are passing in. Luckily the object is passed to the function below, so
   * we can patch the function... */
  if (window.JANRAIN && window.JANRAIN.SSO && window.JANRAIN.SSO.check_login_dispatch) {
    var oldfn = window.JANRAIN.SSO.check_login_dispatch;
    window.JANRAIN.SSO.check_login_dispatch = (a, b) => {
      a.nologin_callback = "nologin_callback";
      oldfn(a, b);
    };
  }
  return window.JANRAIN ? config : null;
}
