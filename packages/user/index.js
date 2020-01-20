require('./build/Login.css');
require('./build/Registration.css');
require('./build/Button.css');
require('./build/ksf-utils.css');
require('./build/InputField.css');
import {jsLoginForm, jsLogout} from './build.js';
import 'basscss/css/basscss-cp.css';

module.exports = {
  Login: jsLoginForm,
  logout: jsLogout,
}
