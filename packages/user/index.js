require("./build/Login.css");
require("./build/Registration.css");
require("./build/Button.css");
require("./build/ksf-utils.css");
require("./build/InputField.css");
import { jsLoginForm, jsLogout, jsMagicLogin } from "./build.js";

module.exports = {
  Login: jsLoginForm,
  logout: jsLogout,
  magicLogin: jsMagicLogin,
};
