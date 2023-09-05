module Vetrina.Purchase.SubscriptionExists where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)

type Props =
  { onClose :: Effect Unit
  , extraMsg :: JSX
  }

subscriptionExists :: Props -> JSX
subscriptionExists props =
  DOM.div
    { className: "vetrina--subscription-exists min-h-[400px] max-h-[690px] flex flex-col justify-center items-center content-center col-span-3"
    , children:
       [ DOM.h1
           { className: "vetrina--subscription-exists-headline font-duplexsans p-5"
           , children: [ DOM.text "Du har redan en prenumeration" ]
           }
       , props.extraMsg
       , closeButton props
       ]
    }

closeButton :: Props -> JSX
closeButton props =
  DOM.button
    { className: "vetrina--button vetrina--completed-close bg-neutral text-white text-lg w-[80%] max-w-[400px] mx-[10%] mb-20 font-duplexsans font-normal py-0.5 px-11 border-neutral rounded cursor-pointer"
    , children: [ DOM.text "Fortsätt läsa artikeln" ]
    , onClick: handler_ props.onClose
    }
