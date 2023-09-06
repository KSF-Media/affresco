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
import KSF.Helpers as Helpers
import KSF.InputField as InputField
import KSF.Paper as Paper
import KSF.Paper (Paper(..))
import KSF.PaymentMethod (paymentMethodOption)
import KSF.Sentry as Sentry
import KSF.User (PaymentMethod(..))
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import KSF.Window (clearOpener)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Component, useState, (/\))
import React.Basic.Hooks as React
import Vetrina.Purchase.NewPurchase.Order (Props, PurchaseInput, createNewAccount, loginToExistingAccount, mkPurchase)
import Vetrina.Types (AccountStatus(..), ExistingAccountForm, FormInputField(..), NewAccountForm, OrderFailure, Product, PurchaseParameters)
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

type State =
  { emailAddress        :: Maybe String
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
  DOM.div
    { className: "vetrina--new-purchase-container flex flex-col items-stretch col-span-3" <>
                 case props.accountStatus of
                       NewAccount -> " vetrina--new-account-container"
                       ExistingAccount _ -> " vetrina--existing-account-container p-5"
                       LoggedInAccount _ -> " vetrina--loggedin-account-container p-5"
    , children:
      [ newPurchaseLinks props
      , case props.accountStatus of
          NewAccount  -> image props
          _ -> mempty
      , title props
      , case props.accountStatus of
          LoggedInAccount user
            | isNothing $ toMaybe user.firstName ->
              DOM.div
                { className: "vetrina--new-purchase-temporary-user-email font-duplexsans font-light pt-1 px-5 border-neutral border-r-2 border-l-2"
                , children: [ DOM.text user.email ]
                }
          _ -> mempty
      , case props.accountStatus of
          NewAccount -> mempty
          _ -> description props
      , productDescription props state
      , form props state setState onSubmit
      , links props
      ]
    }

image :: Props -> JSX
image props = DOM.div
          { className: "vetrina--new-purchase-bg-img h-28 bg-no-repeat bg-center bg-contain"
          --Tailwind doesn't show the background image, therefore it is inline CSS, Tailwind does show it as an img but this should be a background image
          , style: DOM.css { backgroundImage: imgUrl props.paper }
          }
  where
    imgUrl paper =
      case paper of
        Just HBL -> "url('https://cdn.ksfmedia.fi/assets/images/subscribe-paywall-icon-hbl.svg')"
        Just ON  -> "url('https://cdn.ksfmedia.fi/assets/images/subscribe-paywall-icon-on.svg')"
        Just VN  -> "url('https://cdn.ksfmedia.fi/assets/images/subscribe-paywall-icon-vn.svg')"
        _   -> mempty

title :: Props -> JSX
title props =
  let headlineText =
        case props.accountStatus of
          ExistingAccount _    -> Just $ DOM.text "Du har redan ett konto"
          LoggedInAccount user -> Just $ DOM.text $ "Hej " <> (fromMaybe "" $ toMaybe user.firstName)
          NewAccount -> props.headline
  in foldMap headline headlineText
  where
    headline child =
      DOM.div
        { className: "vetrina--headline-" <> maybe "KSF" Paper.toString props.paper <>
                     case props.accountStatus of
                       NewAccount -> " vetrina--new-purchase-headline font-duplexserif text-center px-3 my-3 text-3xl leading-tight font-semibold"
                       ExistingAccount _ -> " vetrina--headline-existing-account font-duplexsans max-w-[600px] w-full font-duplexsans self-center text-center pt-2 border-neutral border-t-2 border-r-2 border-l-2"
                       LoggedInAccount _ -> " vetrina--headline-loggedin-account font-duplexsans font-light pt-5 px-5 border-neutral border-t-2 border-r-2 border-l-2"
        , _data: Object.fromFoldable $ case props.accountStatus of
                   NewAccount -> mempty
                   _          -> [ Tuple.Tuple "existing-account" "1" ]
        , children: [ child ]
        }

description :: Props -> JSX
description props =
  DOM.p
    { className: "vetrina--new-purchase-description-text" <>
                 case props.accountStatus of
                       ExistingAccount _ -> " vetrina--new-purchase-description-text-existing-account w-full max-w-[600px] self-center font-duplexsans text-center pb-2 border-neutral border-r-2 border-b-2 border-l-2"
                       LoggedInAccount _ -> " vetrina--new-purchase-description-text-loggedin-account font-duplexsans font-normal p-5 border-neutral border-r-2 border-l-2"
                       NewAccount        -> mempty
    , children: Array.singleton $
        case props.accountStatus of
          NewAccount        -> mempty
          ExistingAccount _ -> DOM.text "Vänligen logga in med ditt lösenord."
          LoggedInAccount _ -> DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
      }

productDescription :: Props -> State -> JSX
productDescription props state =
  -- Don't show the product selection if we are asking the user to login
  if isNothing state.productSelection
    then mempty
    else case props.accountStatus of
      NewAccount -> foldMap _.description state.productSelection
      LoggedInAccount _ -> foldMap _.descriptionLoggedInAccount state.productSelection
      _ -> mempty

form :: Props -> State -> ((State -> State) -> Effect Unit) -> EventHandler -> JSX
form props state setState onSubmit = DOM.form $
  { className: "vetrina--new-purchase-form flex flex-col justify-center items-center content-center px-5"
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
      , state.errorMessage
      , emailInput props state setState
      ] <> children
  }
  where
    children = case props.accountStatus of
        NewAccount ->
          [ additionalFormRequirements props.accountStatus
          , maybe mempty coverReservationText state.productSelection
          , formSubmitButton props state
          ]
        ExistingAccount _ ->
          [ passwordInput state setState
          , formSubmitButton props state
          ]
        LoggedInAccount _ ->
          [ formSubmitButton props state ]

    additionalFormRequirements NewAccount = acceptTerms
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
    ExistingAccount _ -> resetPasswordLink
    _                 -> mempty
  where
    resetPasswordLink =
      DOM.div
            { className: "vetrina--new-purchase-links font-duplexsans text-center mb-3"
            , children: mkLink "Glömt lösenordet?" "https://konto.ksfmedia.fi/#lösenord" "Klicka här" " text-neutral"
            }

loginLink :: Props -> JSX
loginLink props =
  DOM.div
    { className: "vetrina--new-purchase-link bg-neutral font-duplexsans font-light text-white text-center text-lg leading-tight py-2"
    , children:
        [ DOM.p
          { className: "vetrina--new-purchase-login-text"
          , children: [ DOM.text "Detta är en låst artikel för prenumeranter. "]
          }
        , DOM.span
          { className: "vetrina--new-purchase-login-text"
          , children: [ DOM.text "Redan prenumerant? "]
          }
        , DOM.a
            { className:"vetrina--login-callback cursor-pointer underline"
            , children: [ DOM.text "Logga in här." ]
            , onClick: props.onLogin
            }
        ]
    }

subscribePagesLink :: Array JSX
subscribePagesLink =
  mkLink "" "https://prenumerera.ksfmedia.fi/" "Övriga prenumerationer och betalningssätt" ""

mkLink :: String -> String -> String -> String -> Array JSX
mkLink linkDescription href linkText linkClass = Array.singleton $
  DOM.span_
    [ DOM.text $ linkDescription <> " "
    , DOM.a
        { className: "vetrina--link underline" <> linkClass
        , href
        , children: [ DOM.text linkText ]
        , target: "_blank"
        }
    ]

formSubmitButton :: Props -> State -> JSX
formSubmitButton props state =
  DOM.input
    { type: "submit"
    , className: "vetrina--submit-button bg-neutral text-white text-lg w-[80%] max-w-[400px] min-w-[250px] mx-[10%] mt-5 font-duplexsans font-normal py-0.5 px-11 border-neutral rounded cursor-pointer" <>
                 case props.accountStatus of
                  NewAccount        -> " vetrina--submit-button-new-account mb-20"
                  ExistingAccount _ -> " vetrina--submit-button-existing-account mb-10"
                  LoggedInAccount _ -> " vetrina--submit-button-loggedin-account"
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
    { className: "vetrina--input-wrapper vetrina--with-label text-base w-full max-w-[400px]"
    , children:
        [ InputField.inputField
            { type_: InputField.Email
            , label: Just "Fyll i din e-post för att börja:"
            , name: "emailAddress"
            , placeholder: "Fyll i din e-postadress"
            , onChange: onChange
            , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress state.emailAddress state.serverErrors
            , value: state.emailAddress
            , inputClass: "border mt-1 p-2"
            , extraClass: "font-duplexsans font-light flex flex-col pt-4 pb-10"
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
    { className: "vetrina--input-wrapper vetrina--with-label text-base w-full max-w-[400px]"
    , children:
        [ InputField.inputField
            { type_: InputField.Password
            , placeholder: "Fyll i ditt lösenord:"
            , label: Just "Lösenord"
            , name: "password"
            , value: state.password
            , onChange: \pw -> setState _ { password = pw }
            , validationError:
              Form.inputFieldErrorMessage $
              Form.validateField Password state.password []
            , inputClass: "border mt-1 p-2"
            , extraClass: "font-duplexsans font-light flex flex-col pb-8"
            }
        ]
    }


acceptTerms :: JSX
acceptTerms =
  DOM.div
    { className: "vetrina--terms-conditions max-w-[400px] font-duplexsans font-light text-center text-base leading-tight mb-4"
    , children:
      [ DOM.text "Genom att klicka på \"Vidare\" godkänner du Hufvudstadsbladet Abs " ]
          <> mkLink "" "https://www.hbl.fi/sida/bruksvillkor" "prenumerationsvillkor" " text-neutral"
          <> [ DOM.text " och " ]
          <> mkLink "" "https://www.ksfmedia.fi/dataskydd" "personuppgiftspolicy" " text-neutral"
          <> [ DOM.text ". ", DOM.br {} ]
          <> [ DOM.text "Du uppger kortuppgifter i nästa steg." ]
    }

coverReservationText :: Product -> JSX
coverReservationText product =
  let campaignPrice = case _.priceEur <$> product.campaign of
                        Just campaignPriceEur -> Int.ceil $ campaignPriceEur * 100.0
                        _ -> product.priceCents
  in showCoverReservationText campaignPrice
  where
   showCoverReservationText campaignPrice =
    if campaignPrice > 0
    then mempty
    else
    DOM.div
      { className: "vetrina--cover-reservation max-w-[400px] text-center text-sm font-light font-duplexsans leading-tight mb-4"
      , children:
        [ DOM.text "På kortet görs en täckningsreservering på en euro för att bekräfta att kortet är giltigt. Den här summan debiteras inte från kortet." ]
      }

newAccountFormValidations :: State -> Form.ValidatedForm FormInputField NewAccountForm
newAccountFormValidations state =
  { emailAddress: _
  , productSelection: _
    -- TODO: Validate this and show error message. We are checking this on server side and with
    -- default browser validation. However, a custom JS validation is missing.
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

