module Vetrina.Purchase.NewPurchase where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find, head, length, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldMap, for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.Tuple as Tuple
import Data.Validation.Semigroup (toEither, validation)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import KSF.Api.Package (toSwedish)
import KSF.Helpers (formatEur)
import KSF.Helpers as Helpers
import KSF.InputField as InputField
import KSF.Paper as Paper
import KSF.PaymentMethod (paymentMethodOption)
import KSF.Sentry as Sentry
import KSF.User (PaymentMethod(..))
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import KSF.Window (clearOpener)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState, (/\))
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (EventHandler, handler, handler_)
import Vetrina.Purchase.NewPurchase.Order (Props, PurchaseInput, createNewAccount, loginToExistingAccount, mkPurchase)
import Vetrina.Types (AccountStatus(..), ExistingAccountForm, FormInputField(..), NewAccountForm, OrderFailure, Product, ProductContent, PurchaseParameters)
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

type State =
  { emailAddress        :: Maybe String
  , acceptLegalTerms    :: Boolean
  , password            :: Maybe String
  , serverErrors        :: Array (Form.ValidationError FormInputField)
  , errorMessage        :: JSX
  , productSelection    :: Maybe Product
  , paymentMethod       :: Maybe PaymentMethod
  , showProductContents :: Boolean
  }

component :: Sentry.Logger -> Component Props
component logger = do
  window <- Web.HTML.window
  React.component "NewPurchase" $ \props -> React.do
    state /\ setState <- useState
      { emailAddress: case props.accountStatus of
           ExistingAccount email -> Just email
           _                     -> Nothing
      , acceptLegalTerms: false
      , password: Nothing
      , serverErrors: []
      , errorMessage: mempty
        -- If there's already a selected product, pick that
        -- or take the first item on the products list
      , productSelection: props.productSelection <|> head props.products
      , paymentMethod: props.paymentMethod
      , showProductContents: false
      }
    let onSubmit = handler preventDefault $ \_ -> do
          let withWindow :: forall r. (PurchaseInput r ->  Aff (Either OrderFailure User.User))
                            -> PurchaseInput r -> Effect Unit
              withWindow accountAction validForm = do
                w <- Window.open "" "_blank" "" window
                for_ w clearOpener
                mkPurchase props logger w props.askAccountAlways validForm $ accountAction validForm
          case props.accountStatus of
            -- TODO: Validate `acceptLegalTerms` of `NewAccountForm`
            NewAccount ->
              validation
                (\_ ->
                  setState _
                  { emailAddress = state.emailAddress <|> Just "" })
                (withWindow $ createNewAccount <<< _.emailAddress)
                $ newAccountFormValidations state
            ExistingAccount _ ->
              validation
                (\_ ->
                  setState _
                  { emailAddress = state.emailAddress <|> Just ""
                  , password     = state.password     <|> Just ""
                  })
                (withWindow $ \validForm -> loginToExistingAccount logger
                                            validForm.emailAddress
                                            validForm.password)
                $ existingAccountFormValidations state
            LoggedInAccount user ->
              validation
                (\_ -> pure unit)
                (withWindow $ const $ pure $ Right user)
                $ loggedInAccountFormValidations state

    pure $ case props.customRender of
      Nothing -> render props state setState onSubmit
      Just f -> f (form props state setState onSubmit) props.accountStatus

render :: Props -> State -> ((State -> State) -> Effect Unit) -> EventHandler -> JSX
render props state setState onSubmit =
  title props
  <> newPurchaseLinks props
  <> case props.accountStatus of
    LoggedInAccount user
      | isNothing $ toMaybe user.firstName ->
        DOM.div
          { className: "vetrina--temporary-user-email"
          , children: [ DOM.text user.email ]
          }
    _ -> mempty
  <> case props.accountStatus of
       NewAccount -> mempty
       _ -> description props
  <> form props state setState onSubmit
  <> links props
  <> if length props.products == 1
     then productInformation state setState
     else mempty

title :: Props -> JSX
title props =
  let headlineText =
        case props.accountStatus of
          ExistingAccount _    -> Just $ DOM.text "Du har redan ett KSF Media-konto"
          LoggedInAccount user -> Just $ DOM.text $ "Hej " <> (fromMaybe "" $ toMaybe user.firstName)
          NewAccount -> props.headline
  in foldMap headline headlineText
  where
    headline child =
      DOM.div
        { id: "tb-paywall--headline-" <> maybe "KSF" Paper.toString props.paper
        , className: "vetrina--headline-" <> maybe "KSF" Paper.toString props.paper <>
                     case props.accountStatus of
                       NewAccount -> mempty
                       _          -> " vetrina--headline-existing-account"
        , _data: Object.fromFoldable $ case props.accountStatus of
                   NewAccount -> mempty
                   _          -> [ Tuple.Tuple "existing-account" "1" ]
        , children: [ child ]
        }

description :: Props -> JSX
description props =
  DOM.p
    { id: "tb-paywall--description-text-" <> maybe "KSF" Paper.toString props.paper
    , className: "vetrina--description-text" <>
                 case props.accountStatus of
                       LoggedInAccount _ -> " vetrina--description-text-existing-account"
                       _                 -> mempty
    , children: Array.singleton $
        case props.accountStatus of
          NewAccount        -> mempty
          ExistingAccount _ -> DOM.text "Vänligen logga in med ditt KSF Media lösenord."
          LoggedInAccount _ -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
      }

form :: Props -> State -> ((State -> State) -> Effect Unit) -> EventHandler -> JSX
form props state setState onSubmit = DOM.form $
  { className: "vetrina--form"
  , onSubmit
    -- NOTE: We need to have `emailInput` here (opposed to in `children`),
    -- as we don't want to re-render it when `accountStatus` changes.
    -- This will keep cursor focus in the input field.
  , children:
      -- Show dropdown if more than one product
      [ if length props.products > 1
        then productDropdown props.products
        else mempty
      , renderPaymentMethods props.paymentMethods
       -- Don't show the product selection if we are asking the user to login
      , if not isExistingAccount props.accountStatus
           || isNothing state.productSelection
        then foldMap _.description state.productSelection
        else mempty
      , state.errorMessage
      , emailInput props state setState
      ] <> children
  }
  where
    isExistingAccount (ExistingAccount _) = true
    isExistingAccount _ = false

    children = case props.accountStatus of
        NewAccount ->
          [ additionalFormRequirements props.accountStatus
          , formSubmitButton props state
          ]
        ExistingAccount _ ->
          [ passwordInput state setState
          , formSubmitButton props state
          ]
        LoggedInAccount _ ->
          [ formSubmitButton props state ]

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
              `snoc` (if state.paymentMethod == Just PaperInvoice then paperInvoiceFee else mempty)
          }
      else mempty
      where
        mkPaymentMethodOption p =
          paymentMethodOption
          (\newPaymentMethod -> do
              props.onPaymentMethodChange newPaymentMethod
              setState _ { paymentMethod = newPaymentMethod })
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
                 in setState _ { productSelection = foundProduct }
          , value: fromMaybe "" $ _.id <$> state.productSelection
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
newPurchaseLinks :: Props -> JSX
newPurchaseLinks props =
  case props.accountStatus of
    NewAccount -> loginLink props
    _ -> mempty

links :: Props -> JSX
links props =
  case props.accountStatus of
    NewAccount        -> mempty -- Login link shown elsewhere
    ExistingAccount _ -> linksDiv $ resetPasswordLink <> subscribePagesLink
    LoggedInAccount _ -> linksDiv $ subscribePagesLink
  where
    linksDiv linksJsx =
      DOM.div
        { className: "vetrina--links"
        , children: linksJsx
        }

resetPasswordLink :: Array JSX
resetPasswordLink =
  mkLink "Glömt lösenordet?" "https://konto.ksfmedia.fi/#lösenord" "Klicka här"

loginLink :: Props -> JSX
loginLink props =
  DOM.div
    { className: "vetrina--login-link"
    , children:
        [ DOM.p
          { className: "vetrina--login-text"
          , children: [ DOM.text "Detta är en låst artikel för prenumeranter. "]
          }
        , DOM.span
          { className: "vetrina--login-text"
          , children: [ DOM.text "Redan prenumerant? "]
          }
        , DOM.span
            { className:"vetrina--login-callback"
            , children: [ DOM.text "Logga in här" ]
            , onClick: props.onLogin
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

formSubmitButton :: Props -> State -> JSX
formSubmitButton props state =
  DOM.input
    { type: "submit"
    , className: "vetrina--button"
    , disabled
    , value
    }
  where
    value = case props.accountStatus of
      NewAccount        -> "VIDARE"
      ExistingAccount _ -> "Logga in"
      LoggedInAccount _ -> "Bekräfta och gå vidare"
    disabled = case props.accountStatus of
      NewAccount        -> isFormInvalid $ newAccountFormValidations state
      ExistingAccount _ -> isFormInvalid $ existingAccountFormValidations state
      LoggedInAccount _ -> isFormInvalid $ loggedInAccountFormValidations state

isFormInvalid :: forall a. Form.ValidatedForm FormInputField a -> Boolean
isFormInvalid validations
  | Left errs <- toEither validations
  = not $ all isNotInitialized errs
  | otherwise = false

formatErrorMessage :: String -> JSX
formatErrorMessage = InputField.errorMessage

emailInput :: Props -> State -> ((State -> State) -> Effect Unit) -> JSX
emailInput {accountStatus: (LoggedInAccount _)} _ _ = mempty
emailInput props state setState =
  DOM.div
    { className: "vetrina--input-wrapper vetrina--with-label"
    , children:
        [ InputField.inputField
            { type_: InputField.Email
            , label: Just "Fyll i din e-post för att börja:"
            , name: "emailAddress"
            , placeholder: "Fyll i din e-postadress"
            , onChange: onChange
            , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress state.emailAddress state.serverErrors
            , value: state.emailAddress
            }
        ]
    }
  where
    onChange = case props.accountStatus of
      NewAccount -> \val ->
         setState _
           { emailAddress = val
           , serverErrors = Form.removeServerErrors EmailAddress state.serverErrors
           , errorMessage = mempty
           }
      ExistingAccount _ -> \val -> do
        -- If email value is changed, we must consider it as another
        -- attempt of creating a new account (if an account with this email exists,
        -- and we are asking the user to log in right now, changing the email should cancel that)
        props.onEmailChange
        setState _
          { emailAddress = val
          , password = Nothing
          , serverErrors = Form.removeServerErrors EmailAddress state.serverErrors
          , errorMessage = mempty
          }
      _ -> mempty

passwordInput :: State -> ((State -> State) -> Effect Unit) -> JSX
passwordInput state setState =
  DOM.div
    { className: "vetrina--input-wrapper vetrina--with-label"
    , children:
        [ InputField.inputField
            { type_: InputField.Password
            , placeholder: "Fyll i ditt lösenord"
            , label: Just "Lösenord"
            , name: "password"
            , value: state.password
            , onChange: \pw -> setState _ { password = pw }
            , validationError:
              Form.inputFieldErrorMessage $
              Form.validateField Password state.password []
            }
        ]
    }


acceptTermsCheckbox :: JSX
acceptTermsCheckbox =
  let id    = "accept-terms"
      label =
        DOM.span_ $
          [ DOM.text "Jag godkänner KSF Medias " ]
          <> mkLink "" "https://www.hbl.fi/sida/bruksvillkor" "användarvillkor"
          <> [ DOM.text " och bekräftar att jag har läst och förstått " ]
          <> mkLink "" "https://www.ksfmedia.fi/dataskydd" "integritets-policyn"

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

productInformation :: State -> ((State -> State) -> Effect Unit) -> JSX
productInformation state setState =
  DOM.div
    { className: "vetrina--product-container"
    , children: Array.singleton $
        DOM.div
          { className: "vetrina--product-information"
          , children:
              [ DOM.div
                  { className: "vetrina--product-information__headline"
                  , onClick: handler_ $ setState _ { showProductContents = not state.showProductContents }
                  , children:
                      [ DOM.span
                         { className: "vetrina--product-information__name"
                         , children: [ DOM.text $ foldMap _.name state.productSelection ]
                         }
                     , DOM.span
                         { className: "vetrina--product-information__description"
                         , children:
                             [ DOM.text $ foldMap (formatEur <<< _.priceCents) state.productSelection
                             , DOM.text "€/månad" -- TODO: Always maybe not month
                             ]
                         }
                     , DOM.span
                         { className: "vetrina--product-information__arrow-"
                                      <> if state.showProductContents
                                         then "down"
                                         else "up"
                         }
                     ]
                  }
              ] <> if state.showProductContents
                   then (foldMap (map renderProductContents) $ _.contents <$> state.productSelection)
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

newAccountFormValidations :: State -> Form.ValidatedForm FormInputField NewAccountForm
newAccountFormValidations state =
  { emailAddress: _
  , productSelection: _
    -- TODO: Validate this and show error message. We are checking this on server side and with
    -- default browser validation. However, a custom JS validation is missing.
  , acceptLegalTerms: state.acceptLegalTerms
  , paymentMethod: _
  }
  <$> Form.validateField EmailAddress state.emailAddress []
  <*> (Form.validateField ProductSelection (map _.id state.productSelection) [] *> pure state.productSelection)
  <*> (Form.validateField PaymentMethod (map show state.paymentMethod) [] *> pure state.paymentMethod)

existingAccountFormValidations :: State -> Form.ValidatedForm FormInputField ExistingAccountForm
existingAccountFormValidations state =
  { emailAddress: _
  , password: _
  , productSelection: _
  , paymentMethod: state.paymentMethod
  }
  <$> Form.validateField EmailAddress state.emailAddress []
  <*> Form.validateField Password state.password []
  <*> (Form.validateField ProductSelection (map _.id state.productSelection) [] *> pure state.productSelection)

loggedInAccountFormValidations :: State -> Form.ValidatedForm FormInputField { | PurchaseParameters }
loggedInAccountFormValidations state =
  { productSelection: _
  , paymentMethod: state.paymentMethod
  }
  <$> (Form.validateField ProductSelection (map _.id state.productSelection) [] *> pure state.productSelection)

