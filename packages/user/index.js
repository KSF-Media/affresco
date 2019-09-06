require('./build/Login.css');
require('./build/Registration.css');
require('./build/Button.css');
import {jsLoginForm, jsLogout} from './build.js';
import 'basscss/css/basscss-cp.css';

module.exports = {
  Login: jsLoginForm,
  logout: jsLogout,
}
