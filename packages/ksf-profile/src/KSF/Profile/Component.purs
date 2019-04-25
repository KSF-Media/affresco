module KSF.Profile.Component where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (catMaybes, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import KSF.DescriptionList.Component (Description(..))
import KSF.DescriptionList.Component as DescriptionList
import KSF.Editable.Component (editable)
import KSF.Login.Component as Login
import Persona as Persona
import React.Basic as React
import React.Basic.Extended (JSX, Style, requireStyle)
import Unsafe.Coerce (unsafeCoerce)

foreign import profileStyles :: Style

type Self = React.Self Props State

type Props =
  { profile :: Persona.User
  , onUpdate :: Persona.User -> Effect Unit
  }

type State =
  { name :: Maybe Name
  , address :: Maybe Address
  }

type Name =
  { firstName :: String
  , lastName :: String
  }

type Address =
  { zipCode :: String
  , countryCode :: String
  , streetAddress :: String
  }

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = React.makeStateless component render

addressArray :: Persona.Address -> Array String
addressArray { streetAddress, zipCode, city } = do
  let takeJust = catMaybes <<< map Nullable.toMaybe
  streetAddress : takeJust [ zipCode, city ]


render :: Props -> JSX
render props@{ profile: user } =
  requireStyle
    profileStyles
    $ React.element
        DescriptionList.component
          { definitions:
            [ { term: "Namn:"
              , description: Editable $ editable
                { values: [ fixNullable user.firstName, fixNullable user.lastName ]
                , onSave: saveName
                }
              }
            , { term: "Adress:"
              , description: Editable $ editable
                { values: address
                , onSave: saveAddress
                }
              }
            , { term: "E-postadress:", description: Static [ user.email ] }
            , { term: "Kundnummer:", description: Static [ user.cusno ] }
            ]
          }
  where
    -- | I'm sorry
    fixNullable :: Nullable String -> String
    fixNullable a = fromMaybe "" $ Nullable.toMaybe a

    address = fromMaybe [] $ addressArray <$> Nullable.toMaybe user.address

    saveName :: Effect Unit -> Array String -> Effect Unit
    saveName onError arr = do
      loginResponse <- Login.loadToken
      case loginResponse, arr of
        Just { token }, [firstName, lastName] -> do
          let body = Persona.UpdateName { firstName, lastName }
          Aff.launchAff_ do
            newUser <- Persona.updateUser user.uuid token body `catchError` \err -> do
              Console.error "Unexpected error when updating name."
              liftEffect $ onError
              throwError err
            liftEffect $ props.onUpdate newUser
        _, _ -> Console.error "Did not find token in local storage."

    -- TODO: persona
    -- TODO: remember to pass the country code back
    saveAddress onError arr = Aff.launchAff_ do
      liftEffect $ log $ unsafeCoerce arr
