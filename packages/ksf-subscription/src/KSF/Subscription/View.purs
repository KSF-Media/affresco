module KSF.Subscription.View where

import Prelude

import Data.Array (foldMap)
import Data.Maybe (Maybe)
import KSF.DescriptionList.Component (Description(..))
import KSF.DescriptionList.Component as DescriptionList
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React

foreign import subscriptionStyles :: Style

type Attributes =
  { product :: String
  , status :: String
  , nextBillingDate :: Maybe String
  }

subscription :: Attributes -> JSX
subscription { product, status, nextBillingDate } =
  React.requireStyle
    subscriptionStyles
    $ React.element
        DescriptionList.component
          { definitions:
              [ { term: "Produkt:"
                , description: Static [ product ]
                }
              , { term: "Status:"
                , description: Static [ status ]
                }
              ]
              <> foldMap billingDateTerm nextBillingDate
          }
  where
    billingDateTerm date =
      [ { term: "Nästa faktureringsdatum:"
        , description: Static [ date ]
        }
      ]
