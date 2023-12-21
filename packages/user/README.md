# ksf-media/user
 
User related functions for Hufvudstadsbladet Ab apps.
Has two important features:
- `User` module (PS only), which has functions for e.g. creating and editing a user
- `Login` react component (PS/JS), which is a ready-made component for logging into Hufvudstadsbladet Ab apps

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
The `Login` component offers one way of logging users in:
- Persona (Hufvudstadsbladet Ab's own login service)

Depending on the production environment we're in (dev, prod), the configuration settings for these features might differ.

`Login` expects the following configuration variables to be present:
- `PERSONA_URL`

[dotenv](https://github.com/motdotla/dotenv) is used for setting the variables in place.

### Logging out
It's easy. The `logout` function takes two callbacks:
1) function to call when logout is successful
2) function to call when logout fails

```javascript
import { logout } from '@ksf-media/user';

function onLogoutSuccess() {
  console.log("Logged out successfully!");
}
function onLogoutFail(errorString) {
  console.log("Logging out failed: ", errorString);
}
logout(onLogoutSuccess, onLogoutFail);
```
