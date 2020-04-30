# ksf-media/vetrina
 
## For javascript use

Install it from npm `yarn add @ksf-media/vetrina` and just import it into your project: 
```javascript
import ReactDOM from 'react-dom';
import { Vetrina } from '@ksf-media/vetrina';
var renderVetrina = function() {
  ReactDOM.render(
    <Vetrina products={[{id: "HBL", description: "HBL is amazing!", priceCents: 990 }]} />
    , document.getElementById('vetrina'));
};
```

### Props
The props/callbacks for `Vetrina` are
- onClose
    - this is called when the purchase is done and should be probably used for closing Vetrina
    - `() => { console.log("Purchase done!") }`
- onLogin
    - used for invoking whatever functionality the embedding part has for logging in users
    - `() => { openLoginModal() }`
- products
    - an array of `Products` we want to be subscribeable, see type of `Product` below
    - `products={[id: "HBL", description: "HBL is amazing!", priceCents: 990 }]}`

NOTE: Currently, the `products` array only supports one element.

### Product

```purescript
type Product =
  { id          :: String -- The package id of the product
  , description :: Array String -- Description is an array of strings to show. Every string of this array starts in a new line
  , priceCents  :: Int -- The product price in cents
  }
```

The `Product` must have each attribute defined.

### Getting it up and running

Depending on the production environment we're in (dev, prod), the configuration settings for these features might differ.

`Vetrina` expects the following configuration variables to be present:
- `PERSONA_URL`
- `BOTTEGA_URL`

[dotenv](https://github.com/motdotla/dotenv) is used for setting the variables in place.

