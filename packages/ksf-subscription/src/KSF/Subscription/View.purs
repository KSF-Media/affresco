module KSF.Subscription.View where

import Prelude

import Data.Array (foldMap)
import Data.Maybe (Maybe)
import KSF.DescriptionList.Component as DescriptionList
import Persona as Persona
import React.Basic.DOM as DOM
import React.Basic.Extended (JSX, Style)
import React.Basic.Extended as React

foreign import subscriptionStyles :: Style

type Attributes =
  { subscription :: Persona.Subscription
  , product :: String
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
                , descriptions: [ product ]
                }
              , { term: "Status:"
                , descriptions: [ status ]
                }
              ]
              <> foldMap billingDateTerm nextBillingDate
          }
          <> pauseSubscription
  where
    billingDateTerm date =
      [ { term: "Nästa faktureringsdatum:"
        , descriptions: [ date ]
        }
      ]

    pauseSubscription :: JSX
    pauseSubscription =
      DOM.div
        { className: "subscription--pause-subscription mt2"
        , children:
            [ DOM.a
                { href: "#"
                , children: [ DOM.text "Gör uppehåll" ]
                }
            ]
        }
