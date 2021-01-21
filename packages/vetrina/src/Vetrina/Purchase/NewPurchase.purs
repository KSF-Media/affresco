module Vetrina.Purchase.NewPurchase where

import Prelude

import Control.Alt ((<|>))
import Data.Array (all, find, head, length, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.Tuple as Tuple
import Data.Validation.Semigroup (toEither, unV, invalid)
import Effect (Effect)
import KSF.Api.Package (toSwedish)
import KSF.Helpers (formatEur)
import KSF.Helpers as Helpers
import KSF.InputField as InputField
import KSF.Paper (Paper)
import KSF.Paper as Paper
import KSF.PaymentMethod (paymentMethodOption)
import KSF.User (PaymentMethod(..), User)
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic.Classic (JSX, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import Vetrina.Types (AccountStatus(..), Product, ProductContent)

type Self = React.Self Props State

type State =
  { newAccountForm ::
       { emailAddress     :: Maybe String
       , acceptLegalTerms :: Boolean
       }
  , existingAccountForm ::
       { emailAddress :: Maybe String
       , password     :: Maybe String
       }
  , accountStatus       :: AccountStatus
  , serverErrors        :: Array (Form.ValidationError FormInputField)
  , errorMessage        :: JSX
  , productSelection    :: Maybe Product
  , paymentMethod       :: Maybe PaymentMethod
  , showProductContents :: Boolean
  }

type Props =
  { accountStatus                 :: AccountStatus
  , products                      :: Array Product
  , errorMessage                  :: Maybe String
  , mkPurchaseWithNewAccount      :: NewAccountForm -> Effect Unit
  , mkPurchaseWithExistingAccount :: ExistingAccountForm -> Effect Unit
  , mkPurchaseWithLoggedInAccount :: User -> { | PurchaseParameters } -> Effect Unit
  , productSelection              :: Maybe Product
  , onLogin                       :: Effect Unit
  , headline                      :: Maybe JSX
  , paper                         :: Maybe Paper
  , paymentMethod                 :: Maybe PaymentMethod -- ^ Pre-selected payment method
  , paymentMethods                :: Array PaymentMethod
  , onPaymentMethodChange         :: Maybe PaymentMethod -> Effect Unit
  , minimalLayout                 :: Boolean
  }

data FormInputField
  = EmailAddress
  | Password
  | ProductSelection
  | PaymentMethod

derive instance eqNewAccountInputField :: Eq FormInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField FormInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    Password         -> Form.validateEmptyField Password "Lösenord krävs." value
    ProductSelection ->
      -- As `validateField` works currently only with `Maybe Strings`, we need to manually
      -- check the value here (for now). The `value` passed here is maybe the productSelection.id
      if isNothing value
      then invalid (NonEmptyList.singleton (Form.InvalidEmpty ProductSelection "")) -- TODO: Do we need an error message here?
      else pure $ Just mempty
    PaymentMethod    -> Form.validateEmptyField PaymentMethod "Betalningssätt krävs." value

type PurchaseParameters =
  ( productSelection :: Maybe Product
  , paymentMethod    :: Maybe User.PaymentMethod
  )

type NewAccountForm =
  { emailAddress     :: Maybe String
  , acceptLegalTerms :: Boolean
  | PurchaseParameters
  }

type ExistingAccountForm =
  { emailAddress :: Maybe String
  , password     :: Maybe String
  | PurchaseParameters
  }

component :: React.Component Props
component = React.createComponent "NewPurchase"

newPurchase :: Props -> JSX
newPurchase props = make component
  { initialState: { newAccountForm:
                      { emailAddress: Nothing
                      , acceptLegalTerms: false
                      }
                  , existingAccountForm:
                      { emailAddress: Nothing
                      , password: Nothing
                      }
                  , accountStatus: NewAccount
                  , serverErrors: []
                  , errorMessage: foldMap formatErrorMessage props.errorMessage
                  , productSelection: Nothing
                  , paymentMethod: Nothing
                  , showProductContents: false
                  }
  , render
  , didMount
  }
  props

didMount :: Self -> Effect Unit
didMount self = do
  let maybeExistingUserEmail =
        case self.props.accountStatus of
          ExistingAccount email -> Just email
          _                     -> Nothing
  self.setState _ { accountStatus = self.props.accountStatus
                  , paymentMethod = self.props.paymentMethod
                  , productSelection =
                      -- If there's already a selected product, pick that
                      -- or take the first item on the products list
                      self.props.productSelection <|> head self.props.products
                  , existingAccountForm { emailAddress = maybeExistingUserEmail }
                  }

render :: Self -> JSX
render self =
  title self
  <> newPurchaseLinks self
  <> case self.state.accountStatus of
    LoggedInAccount user
      | isNothing $ toMaybe user.firstName ->
        DOM.div
          { className: "vetrina--temporary-user-email"
          , children: [ DOM.text user.email ]
          }
    _ -> mempty
  <> case self.state.accountStatus of
       NewAccount -> mempty
       _ -> description self
  <> form self
  <> links self
  -- Do not show product details if minimalLayout
  <> if not self.props.minimalLayout && length self.props.products == 1
     then productInformation self
     else mempty

title :: Self -> JSX
title self =
  let headlineText =
        case self.state.accountStatus of
          ExistingAccount _    -> Just $ DOM.text "Du har redan ett KSF Media-konto"
          LoggedInAccount user -> Just $ DOM.text $ "Hej " <> (fromMaybe "" $ toMaybe user.firstName)
          NewAccount ->
            if self.props.minimalLayout
            then Nothing
            else self.props.headline
  in foldMap headline headlineText
  where
    headline child =
      DOM.h1
        { className: "vetrina--headline-" <> maybe "KSF" Paper.toString self.props.paper
        , children: [ child ]
        }

description :: Self -> JSX
description self =
  -- Do not show logged in text if minimal layout activated
  if self.props.minimalLayout && isLoggedInAccount self.state.accountStatus
  then mempty
  else
    DOM.p
      { className: "vetrina--description-text"
      , children: Array.singleton $
        case self.state.accountStatus of
          NewAccount        -> mempty
          ExistingAccount _ -> DOM.text "Vänligen logga in med ditt KSF Media lösenord."
          LoggedInAccount _ -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
      }
  where
    isLoggedInAccount (LoggedInAccount _) = true
    isLoggedInAccount _ = false

form :: Self -> JSX
form self = DOM.form $
  { className: "vetrina--form"
  , onSubmit
    -- NOTE: We need to have `emailInput` here (opposed to in `children`),
    -- as we don't want to re-render it when `accountStatus` changes.
    -- This will keep cursor focus in the input field.
  , children:
      -- Show dropdown if more than one product OR in any case if minimalLayout is enabled
      [ if length self.props.products > 1 || self.props.minimalLayout
        then productDropdown self.props.products
        else mempty
      , renderPaymentMethods self.props.paymentMethods
       -- Don't show the product selection if we are asking the user to login
       -- or if on minimalLayout
      , if not self.props.minimalLayout
           && not isExistingAccount self.state.accountStatus
           || isNothing self.state.productSelection
        then foldMap _.description self.state.productSelection
        else mempty
      , self.state.errorMessage
      , emailInput self self.state.accountStatus
      ] <> children
  }
  where
    isExistingAccount (ExistingAccount _) = true
    isExistingAccount _ = false

    onSubmit = handler preventDefault $ case self.state.accountStatus of
      NewAccount ->
        (\_ -> unV
          (\errors ->
            self.setState _
              { newAccountForm
                { emailAddress = self.state.newAccountForm.emailAddress <|> Just "" }})
          self.props.mkPurchaseWithNewAccount
          $ newAccountFormValidations self)
      ExistingAccount _ ->
        (\_ -> unV
          (\errors ->
            self.setState _
              { existingAccountForm
                { emailAddress = self.state.existingAccountForm.emailAddress <|> Just ""
                , password     = self.state.existingAccountForm.password     <|> Just ""
                }})
          self.props.mkPurchaseWithExistingAccount
          $ existingAccountFormValidations self)
      LoggedInAccount user ->
        (\_ -> unV
          (\errors -> pure unit)
          (\validForm -> self.props.mkPurchaseWithLoggedInAccount user validForm)
          $ loggedInAccountFormValidations self)
    children = case self.state.accountStatus of
        NewAccount ->
          [ additionalFormRequirements self.state.accountStatus
          , formSubmitButton self
          ]
        ExistingAccount _ ->
          [ passwordInput self
          , formSubmitButton self
          ]
        LoggedInAccount _ ->
          [ formSubmitButton self ]

    additionalFormRequirements NewAccount = acceptTermsCheckbox
    additionalFormRequirements _ = mempty

    renderPaymentMethods :: Array User.PaymentMethod -> JSX
    renderPaymentMethods paymentMethods =
      if length paymentMethods > 1
      then
        DOM.div
          { className: "vetrina--payment-methods"
          , children:
              map mkPaymentMethodOption paymentMethods
              `snoc` (if self.state.paymentMethod == Just PaperInvoice then paperInvoiceFee else mempty)
          }
      else mempty
      where
        mkPaymentMethodOption p =
          paymentMethodOption
          (\newPaymentMethod -> do
              self.props.onPaymentMethodChange newPaymentMethod
              self.setState _ { paymentMethod = newPaymentMethod })
          p
        paperInvoiceFee =
          DOM.div
            { className: "vetrina--paper-invoice-fee"
            , children: [ DOM.text "Obs! På pappersfakturor som levereras per post uppbär vi en tilläggsavgift på 5,00 euro per faktura (inkl. Moms)." ]
            }

    productDropdown :: Array Product -> JSX
    productDropdown products =
      divWithClass "vetrina--input-wrapper"
      $ divWithClass "input-field--container"
      $ DOM.select
          { children: map mkOption products
          , onChange: handler targetValue
              \newProductSelection ->
                 let foundProduct = find ((_ == newProductSelection) <<< Just <<< _.id) products
                 in self.setState _ { productSelection = foundProduct }
          , value: fromMaybe "" $ _.id <$> self.state.productSelection
          }
      where
        mkOption product =
          DOM.option
            { value: product.id
            , children:
                [ DOM.text
                  $ product.name
                  <> ", "
                  <> renderPrice product
                ]
            }
        renderPrice :: Product -> String
        renderPrice product =
          let priceCents =
                -- If campaign given, show that price
                case _.priceEur <$> product.campaign of
                  Just campaignPriceEur -> Int.ceil $ campaignPriceEur * 100.0
                  _ -> product.priceCents
              pricePeriod =
                case product.campaign of
                  Just c ->
                    let monthString = toSwedish c.lengthUnit #
                          if c.length > 1
                          then Tuple.snd -- Plural
                          else Tuple.fst -- Singular
                    in show c.length <> " " <> monthString
                  -- TODO: This should probably be defined in props,
                  -- however currently we are using the one month
                  -- prices of a product
                  _ -> "månad"
          in Helpers.formatEur priceCents <> " € / " <> pricePeriod
        -- | Just a small helper as we need to wrap this thing inside two divs
        divWithClass className child = DOM.div { className, children: [ child ] }

-- | Only show this when initial screen with new account
newPurchaseLinks :: Self -> JSX
newPurchaseLinks self =
  case self.state.accountStatus of
    NewAccount -> loginLink self
    _ -> mempty

links :: Self -> JSX
links self =
  if self.props.minimalLayout
  then mempty
  else
    case self.state.accountStatus of
      NewAccount        -> mempty -- Login link shown elsewhere
      ExistingAccount _ -> linksDiv $ resetPasswordLink <> subscribePagesLink
      LoggedInAccount _ -> linksDiv $ faqLink <> subscribePagesLink
  where
    linksDiv linksJsx =
      DOM.div
        { className: "vetrina--links"
        , children: linksJsx
        }

resetPasswordLink :: Array JSX
resetPasswordLink =
  mkLink "Glömt lösenordet?" "https://www.hbl.fi/losenord/" "Klicka här"

faqLink :: Array JSX
faqLink =
  mkLink "Vad är Premium?" "https://www.hbl.fi/fragor-och-svar/" "Frågor och svar"

loginLink :: Self -> JSX
loginLink self =
  if self.props.minimalLayout
  then mempty
  else DOM.span
         { className: "vetrina--login-link"
         , children:
             [ DOM.text "Redan prenumerant? "
             , DOM.span
                 { className:"vetrina--login-callback"
                 , children: [ DOM.text "Logga in för att fortsätta läsa" ]
                 , onClick: handler_ self.props.onLogin
                 }
             ]
         }

subscribePagesLink :: Array JSX
subscribePagesLink =
  mkLink "" "https://prenumerera.ksfmedia.fi/" "Övriga prenumerationer och betalningssätt"

mkLink :: String -> String -> String -> Array JSX
mkLink linkDescription href linkText = Array.singleton $
  DOM.span_
    [ DOM.text $ linkDescription <> " "
    , DOM.a
        { className: "vetrina--link"
        , href
        , children: [ DOM.text linkText ]
        , target: "_blank"
        }
    ]

formSubmitButton :: Self -> JSX
formSubmitButton self =
  DOM.input
    { type: "submit"
    , className: "vetrina--button"
    , disabled
    , value
    }
  where
    value = case self.state.accountStatus of
      NewAccount        -> "Bekräfta och gå vidare"
      ExistingAccount _ -> "Logga in"
      LoggedInAccount _ -> "Bekräfta och gå vidare"
    disabled = case self.state.accountStatus of
      NewAccount        -> isFormInvalid $ newAccountFormValidations self
      ExistingAccount _ -> isFormInvalid $ existingAccountFormValidations self
      LoggedInAccount _ -> isFormInvalid $ loggedInAccountFormValidations self

isFormInvalid :: forall a. Form.ValidatedForm FormInputField a -> Boolean
isFormInvalid validations
  | Left errs <- toEither validations
  = not $ all isNotInitialized errs
  | otherwise = false

formatErrorMessage :: String -> JSX
formatErrorMessage = InputField.errorMessage

emailInput :: Self -> AccountStatus -> JSX
emailInput _ (LoggedInAccount _) = mempty
emailInput self accountStatus =
  let emailValue = self.state.existingAccountForm.emailAddress <|> self.state.newAccountForm.emailAddress
  in DOM.div
     { className: "vetrina--input-wrapper vetrina--with-label"
     , children:
         [ if accountStatus == NewAccount && not self.props.minimalLayout
           then DOM.div
                  { className: "vetrina--step vetrina--create-account"
                  , children:
                      [ DOM.span
                          { className: "vetrina--step__headline"
                          , children: [ DOM.text "Skapa konto" ]
                          }
                      , DOM.text "STEG 1 / 2 KONTOINFORMATION"
                      ]
                  }
           else mempty
         , InputField.inputField
             { type_: InputField.Email
             , label: Just "E-postadress"
             , name: "emailAddress"
             , placeholder: "E-postadress"
             , onChange: onChange
             , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress emailValue self.state.serverErrors
             , value: emailValue
             }
         ]
     }
  where
    onChange = case self.state.accountStatus of
      NewAccount -> \val ->
         self.setState _
           { newAccountForm { emailAddress = val }
           , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
           , errorMessage = mempty
           }
      ExistingAccount _ -> \val ->
        self.setState _
          { existingAccountForm { emailAddress = Nothing, password = Nothing }
          , newAccountForm { emailAddress = val }
            -- If email value is changed, we must consider it as another
            -- attempt of creating a new account (if an account with this email exists,
            -- and we are asking the user to log in right now, changing the email should cancel that)
          , accountStatus = NewAccount
          , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
          , errorMessage = mempty
          }
      _ -> mempty

passwordInput :: Self -> JSX
passwordInput self =
  DOM.div
    { className: "vetrina--input-wrapper vetrina--with-label"
    , children:
        [ InputField.inputField
            { type_: InputField.Password
            , placeholder: "Lösenord"
            , label: Just "Lösenord"
            , name: "password"
            , value: self.state.existingAccountForm.password
            , onChange: \pw -> self.setState _ { existingAccountForm { password = pw } }
            , validationError:
              Form.inputFieldErrorMessage $
              Form.validateField Password self.state.existingAccountForm.password []
            }
        ]
    }


acceptTermsCheckbox :: JSX
acceptTermsCheckbox =
  let id    = "accept-terms"
      label =
        DOM.span_ $
          [ DOM.text "Jag godkänner KSF Medias " ]
          <> mkLink "" "https://www.hbl.fi/bruksvillkor/#terms" "användarvillkor"
          <> [ DOM.text " och bekräftar att jag har läst och förstått " ]
          <> mkLink "" "https://www.hbl.fi/bruksvillkor/#privacy" "integritets-policyn"

  in DOM.div
    { className: "vetrina--checkbox-container"
    , children:
        [ DOM.input
            { className: "vetrina--checkbox"
            , type: "checkbox"
            , id
            , required: true
            }
        , DOM.label
            { className: "vetrina--checkbox-label"
            , htmlFor: id
            , children: [ label ]
            }
        ]
    }

productInformation :: Self -> JSX
productInformation self =
  DOM.div
    { className: "vetrina--product-container"
    , children: Array.singleton $
        DOM.div
          { className: "vetrina--product-information"
          , children:
              [ DOM.div
                  { className: "vetrina--product-information__headline"
                  , onClick: handler_ $ self.setState _ { showProductContents = not self.state.showProductContents }
                  , children:
                      [ DOM.span
                         { className: "vetrina--product-information__name"
                         , children: [ DOM.text $ foldMap _.name self.state.productSelection ]
                         }
                     , DOM.span
                         { className: "vetrina--product-information__description"
                         , children:
                             [ DOM.text $ foldMap (formatEur <<< _.priceCents) self.state.productSelection
                             , DOM.text "€/månad" -- TODO: Always maybe not month
                             ]
                         }
                     , DOM.span
                         { className: "vetrina--product-information__arrow-"
                                      <> if self.state.showProductContents
                                         then "down"
                                         else "up"
                         }
                     ]
                  }
              ] <> if self.state.showProductContents
                   then (foldMap (map renderProductContents) $ _.contents <$> self.state.productSelection)
                   else mempty
          }
    }
  where
    renderProductContents :: ProductContent -> JSX
    renderProductContents productContent =
      DOM.span
        { className: "vetrina--product-information__contents"
        , children:
            [ DOM.strong
                { className: "vetrina--product-information__contents-name"
                , children: [ DOM.text productContent.title ]
                }
            , DOM.span
                { className: "vetrina--product-information__contents-description"
                , children: [ DOM.text productContent.description ]
                }
            , DOM.span
                { className: "vetrina--product-information__checkmark"
                , children: []
                }
            ]
        }

newAccountFormValidations :: Self -> Form.ValidatedForm FormInputField NewAccountForm
newAccountFormValidations self =
  { emailAddress: _
  , productSelection: _
    -- TODO: Validate this and show error message. We are checking this on server side and with
    -- default browser validation. However, a custom JS validation is missing.
  , acceptLegalTerms: self.state.newAccountForm.acceptLegalTerms
  , paymentMethod: _
  }
  <$> Form.validateField EmailAddress self.state.newAccountForm.emailAddress []
  <*> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)
  <*> (Form.validateField PaymentMethod (map show self.state.paymentMethod) [] *> pure self.state.paymentMethod)

existingAccountFormValidations :: Self -> Form.ValidatedForm FormInputField ExistingAccountForm
existingAccountFormValidations self =
  { emailAddress: _
  , password: _
  , productSelection: _
  , paymentMethod: self.state.paymentMethod
  }
  <$> Form.validateField EmailAddress self.state.existingAccountForm.emailAddress []
  <*> Form.validateField Password self.state.existingAccountForm.password []
  <*> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)

loggedInAccountFormValidations :: Self -> Form.ValidatedForm FormInputField { | PurchaseParameters }
loggedInAccountFormValidations self =
  { productSelection: _
  , paymentMethod: self.state.paymentMethod
  }
  <$> (Form.validateField ProductSelection (map _.id self.state.productSelection) [] *> pure self.state.productSelection)
