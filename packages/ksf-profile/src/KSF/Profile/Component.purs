module KSF.Profile.Component where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (trim)
import Effect (Effect)
import Effect.Console (log)
import KSF.DescriptionList.Component (Description(..))
import KSF.DescriptionList.Component as DescriptionList
import KSF.Editable.Component as Editable
import Persona as Persona
import React.Basic (make, StateUpdate(..), runUpdate)
import React.Basic as React
import React.Basic.Extended (JSX, Style, requireStyle)
import Unsafe.Coerce (unsafeCoerce)

type Self = React.Self Props State
type Props =
  { profile :: Persona.User }

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

data Action
  = StartEdit
  | UpdateName (Maybe Name)
  | UpdateAddress (Maybe Address)
  | Save (Maybe Name) (Maybe Address)

jsComponent :: React.Component Props
jsComponent = component

component :: React.Component Props
component = React.createComponent "Profile"

profile :: Props -> JSX
profile = make component { initialState, render }

initialState :: State
initialState =
  { name: Nothing
  , address: Nothing
  }

render :: Self -> JSX
render { props, state } =
  profileView
    { email: props.profile.email
    , customerNumber: props.profile.cusno
    , firstName: props.profile.firstName
    , lastName: props.profile.lastName
    , address: fromMaybe [] $ addressArray <$> Nullable.toMaybe props.profile.address
    }

addressArray :: Persona.Address -> Array String
addressArray { streetAddress, zipCode, city } = do
  let takeJust = catMaybes <<< map Nullable.toMaybe
  streetAddress : takeJust [ zipCode, city ]


displayName :: forall r. { firstName :: Nullable String, lastName :: Nullable String | r } -> String
displayName { firstName, lastName } =
  trim $ fold
    [ fromMaybe "" $ Nullable.toMaybe lastName
    , " "
    , fromMaybe "" $ Nullable.toMaybe firstName
    ]

-- | I'm sorry
fixNullable :: Nullable String -> String
fixNullable a = fromMaybe "" $ Nullable.toMaybe a

update :: Self -> Action -> StateUpdate Props State
update self action = case action of
  StartEdit -> NoUpdate
  UpdateName name -> NoUpdate
  UpdateAddress address -> NoUpdate
  Save maybeName maybeAddress -> NoUpdate

send :: Self -> Action -> Effect Unit
send = runUpdate update

foreign import profileStyles :: Style

type ProfileAttrs =
  { email :: String
  , customerNumber :: String
  , address :: Array String
  , firstName :: Nullable String
  , lastName :: Nullable String
  }

profileView :: ProfileAttrs -> JSX
profileView { email, customerNumber, address, firstName, lastName } =
  requireStyle
    profileStyles
    $ React.element
        DescriptionList.component
          { definitions:
            [ { term: "Namn:"
              , description: Editable $ Editable.editable
                { values: [ fixNullable firstName, fixNullable lastName ], onSave: \a -> log $ unsafeCoerce a }
              }
            , { term: "Adress:", description: Static address }
            , { term: "E-postadress:", description: Static [ email ] }
            , { term: "Kundnummer:", description: Static [ customerNumber ] }
            ]
          }
