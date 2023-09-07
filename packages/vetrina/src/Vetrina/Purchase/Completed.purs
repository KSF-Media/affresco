module Vetrina.Purchase.Completed where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import Vetrina.Types (AccountStatus(..), Product)

type Props =
  { onClose          :: Maybe (Effect Unit)
  , user             :: Maybe User.User
  , accountStatus    :: AccountStatus
  , purchasedProduct :: Maybe Product
  }

completed :: Props -> JSX
completed props =
  DOM.div
    { className: "vetrina--completed-container flex flex-col justify-center w-[90%] max-w-[500px] min-w-[250px]"
    , children:
      [ DOM.h1
          { className: "vetrina--headline"
          , children:
              [ case props.accountStatus of
                   NewAccount      -> DOM.text "Ditt konto är klart!"
                   _               -> DOM.text "Tack för din beställning!"
              ]
          }
      , DOM.p
          { className: "vetrina--description-text-product"
          , children: [ foldMap _.descriptionPurchaseCompleted props.purchasedProduct ]
          }
      , DOM.p
          { className: "vetrina--description-text"
          , children: [ DOM.text $ "Vi har skickat en bekräftelse till " <> maybe "" _.email props.user ]
          }
      ]
    }
  -- If `onClose` is Nothing, let's not render button
  <> foldMap completeButton props.onClose

completeButton :: Effect Unit -> JSX
completeButton onClose =
  DOM.button
    { className: "vetrina--button vetrina--completed-close bg-neutral text-white text-lg w-[80%] max-w-[400px] mx-[10%] mt-5 font-duplexsans font-normal py-0.5 px-11 border-neutral rounded cursor-pointer"
    , children: [ DOM.text "Fortsätt läsa artikeln" ] -- TODO: This text may vary depending on use case
    , onClick: handler_ onClose
    }
