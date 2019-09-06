# ksf-media/user
 
User related functions for KSF Media apps.
Has two important features:
- `User` module (PS only), which has functions for e.g. creating and editing a user
- `Login` react component (PS/JS), which is a ready-made component for logging into KSF Media apps

## For javascript use

Install it from npm `yarn add @ksf-media/user` and just import it into your project: 
```javascript
import ReactDOM from 'react-dom';
import { Login } from '@ksf-media/user';
var renderLogin = function() {
  ReactDOM.render(
    <Login onUserFetchSuccess={(user) => console.log("Success!")} />
    , document.getElementById('login'));
};
```

### Props
The props/callbacks for `Login` are
- onMerge
    - `() => { console.log("request to merge accounts happened") }`
- onMergeCancelled
    - `() => { console.log("merge was canceled") }`
- onRegister
    - `() => { console.log("registration form is rendered") }`
- onRegisterCancelled
    - `() => { console.log("registration is canceled") }`
- onUserFetchFail
    - `(error) => { console.log("fetching user failed!", error) }`
- onUserFetchSuccess
    - `(user) => { console.log("user fetched successfully!", user) }`
- onLoading
    - `() => { showLoadingSpinner = true }`
- onLoadingEnd
    - `() => { showLoadingSpinner = false }`
- disableSocialLogins
    - array of social login providers, where provider belongs to `{"Google", "Facebook"}`
    - `disableSocialLogins={["Facebook"]}`

### Getting it up and running
The `Login` component offers multiple ways off logging users in:
- Persona (KSF Media's own login service)
- Google
- Facebook
- SSO

Depending on the production environment we're in (dev, prod), the configuration settings for these features might differ.

`Login` expects the following configuration variables to be present:
- `JANRAIN_LOGIN_CLIENT_ID`
- `JANRAIN_SSO_SERVER`
- `JANRAIN_FLOW_VERSION`
- `JANRAIN_XD_RECEIVER_PATH`
- `PERSONA_URL`
- `GOOGLE_CLIENT_ID`
- `FACEBOOK_APP_ID`

[dotenv](https://github.com/motdotla/dotenv) is used for setting the variables in place.

For getting the SSO working, an [xd_receiver](https://github.com/KSF-Media/affresco/blob/master/apps/mitt-konto/xd_receiver.html) file should be found under the same domain where the app is running.
The `JANRAIN_XD_RECEIVER_PATH` variable is the path to this file.

### Logging out
It's easy.
```javascript
import { logout } from '@ksf-media/user';

function onLogout() {
  console.log("Logged out successfully!")
}
logout(onLogout);
```
