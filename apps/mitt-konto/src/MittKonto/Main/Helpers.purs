module MittKonto.Main.Helpers where

import Prelude

import Data.Array ((:))
import Data.Foldable (oneOf)
import Data.JSDate (JSDate, parse)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Alert.Component (Alert)
import KSF.Alert.Component as Alert
import KSF.Error as KSF.Error
import KSF.Spinner as Spinner
import MittKonto.Main.Types as Types
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Hook, UseEffect, useEffect)

errorAlert :: Error -> Maybe Alert
errorAlert err = oneOf
  [ do _ <- KSF.Error.networkError err
       pure
         { level: Alert.danger
         , title: "Anslutningen misslyckades."
         , message: "Vänligen kontrollera din internetanslutning och försök om en stund igen."
         }
  , pure
      { level: Alert.warning
      , title: "Något gick fel vid inloggningen."
      , message: "Vänligen försök om en stund igen."
      }
  ]

-- I know, but it's just for accessing locale
readJSDate :: String -> JSDate
readJSDate date = unsafePerformEffect $ parse date

classy
  :: ({ className :: String, children :: Array JSX} -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }

anchor :: String -> String -> Array JSX -> JSX
anchor href description children = DOM.a { href, children: DOM.text description : children, target: "_blank" }

useLoadSpinner :: forall p. Eq p => ((Types.State -> Types.State) -> Effect Unit) -> p -> ((Aff Boolean -> Effect Unit) -> Effect Unit) -> Hook (UseEffect p) Unit
useLoadSpinner setState test spinnerableAction =
  useEffect test do
    spinnerableAction $ \x -> launchAff_ $ Spinner.withSpinner (setState <<< Types.setLoading) do
      success <- x
      when (not success) $
        liftEffect $ setState $ Types.setAlert $ Just
          { level: Alert.warning
          , title: "Laddningen misslyckades."
          , message: "Något gick fel, ta kontakt med kundservice."
          }
    pure $ pure unit
