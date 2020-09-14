# ksf-media/vetrina

## For javascript use

Install it from npm `yarn add @ksf-media/vetrina` and just import it into your project:
```javascript
import ReactDOM from 'react-dom';
import { Vetrina } from '@ksf-media/vetrina';
var renderVetrina = function() {
  ReactDOM.render(
    <Vetrina products={[
      {
        id: "HBL",
        description: <div>HBL is amazing!</div>,
        descriptionPurchaseCompleted: <div>You can now read premium content of HBL.fi</div>,
        priceCents: 990,
        campaignNo: 1234
      }
    ]}/>
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
    - `products={[id: "HBL", description: <div>HBL is amazing!</div>, priceCents: 990 }]}`
- unexpectedError
    - a view to be rendered when an unexpected error occurs (for example, if vetrina fails to initialize entirely)
    - `<div>Error!</div>`
- accessEntitlements
    - a list of entitlement names required to do the action the parent wants
    - if at least one of the entitlements given matches with the user entitlements, vetrina will not proceed with the purchase
    - `[ "hbl-web", "hbl-epaper" ]`

### Product

```purescript
type Product =
  { id                           :: String -- The package id of the product
  , description                  :: JSX -- Description is JSX and it's shown when selecting a product
  , descriptionPurchaseCompleted :: JSX -- A description/guide to show when the purchase is completed
  , priceCents                   :: Int -- The product price in cents
  , campaignNo                   :: Maybe Int -- The campaign used for this product (optional)
  }
```

### Getting it up and running

Depending on the production environment we're in (dev, prod), the configuration settings for these features might differ.

`Vetrina` expects the following configuration variables to be present:
- `PERSONA_URL`
- `BOTTEGA_URL`

[dotenv](https://github.com/motdotla/dotenv) is used for setting the variables in place.
