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
        priceCents: 990
      }
    ]}/>
    , document.getElementById('vetrina'));
};
```

### Props
The props/callbacks for `Vetrina` are
- onClose
    - this is called when the purchase is done and should be probably used for closing Vetrina
    - if missing, the "completed" view is shown with out a button, as sometimes there is no need for a final action
    - `() => { console.log("Purchase done!") }`
- onLogin
    - used for invoking whatever functionality the embedding part has for logging in users
    - `() => { openLoginModal() }`
- products
    - an array of `Products` we want to be subscribeable, see type of `Product` below
    - `products={[id: "HBL", description: <div>HBL is amazing!</div>, priceCents: 990, name: "HBL Premium" }]}`
    - if multiple products are given, a dropdown is shown listing the products (if `minimalLayout=true`, it will be shown even with one product)
- unexpectedError
    - a view to be rendered when an unexpected error occurs (for example, if vetrina fails to initialize entirely)
    - `<div>Error!</div>`
- accessEntitlements
    - a list of entitlement names required to do the action the parent wants
    - if at least one of the entitlements given matches with the user entitlements, vetrina will not proceed with the purchase
    - `[ "hbl-web", "hbl-epaper" ]`
- headline
    - headline to show above Vetrina (JSX)
    - `headline={<div className="vetrina--headline">Läs HBL digitalt för <span className="vetrina--price-headline">endast 1€</span></div>}`
- paper
    - name of the paper (brand) in question
    - `paper` should belong to `("HBL", "VN", "ON", "ÖN", "ÖNY", "KSF")`
    - if `paper` is missing or invalid, the brand neutral `"KSF"` is used
- paymentMethods
    - list of payment methods to choose from, supported methods currently are `"creditcard"` and `"paperinvoice"`
    - `[ "creditcard", "paperinvoice" ]`
    - if missing, `"creditcard"` is used by default
- minimalLayout
    - a boolean to switch minimal layout on or off
    - this will leave most of the copy and product descriptions out
### Product

```purescript
type Product =
  { id                           :: String -- The package id of the product
  , name                         :: String -- Name of the product, e.g. "Hufvudstadsbladet Premium"
  , priceCents                   :: Int -- The product price in cents
  , description                  :: JSX -- Description is JSX and it's shown when selecting a product (optional)
  , descriptionPurchaseCompleted :: JSX -- A description/guide to show when the purchase is completed (optional)
  , campaign                     :: Maybe Campaign -- The campaign used for this product (optional). Note however that if the campaign is somehow defined, but faulty, error will be thrown
  , contents                     :: Array ProductContent -- For describing the product in more detail (optional)
  }

type Campaign =
  { no         :: Int
  , id         :: String
  , name       :: String
  , length     :: Int
  , lengthUnit :: String -- Should belong to `("day", "month", "week", "year")`
  , priceEur   :: Number
  }

type ProductContent =
  { description :: String
  , title       :: String
  }
```

### Getting it up and running

Depending on the production environment we're in (dev, prod), the configuration settings for these features might differ.

`Vetrina` expects the following configuration variables to be present:
- `PERSONA_URL`
- `BOTTEGA_URL`

[dotenv](https://github.com/motdotla/dotenv) is used for setting the variables in place.

### Publishing

Run `npm publish` - this will run [the end-to-end tests](../../apps/vetrina-test/README.md),
which you should fix before publishing.
