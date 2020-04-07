module Vetrina.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array (all, head, intercalate, length)
import Data.Array as Array
import Data.Either (Either(..), hush, isRight, note)
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Nullable (toNullable)
import Data.Validation.Semigroup (toEither, unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import KSF.Api (InvalidateCache(..))
import KSF.Api.Package (PackageName, Package)
import KSF.Api.Package as Package
import KSF.InputField.Component as InputField
import KSF.JSError as Error
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (PaymentMethod(..), User, Order, PaymentTerminalUrl, OrderStatusState(..))
import KSF.User as User
import KSF.ValidatableForm (isNotInitialized)
import KSF.ValidatableForm as Form
import React.Basic (JSX, fragment, make)
import React.Basic as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import Vetrina.Purchase.Completed as Purchase.Completed
import Vetrina.Purchase.SetPassword as Purchase.SetPassword
import Vetrina.Types (AccountStatus(..))

foreign import sentryDsn_ :: Effect String

type Props =
  { onClose  :: Effect Unit
  , products :: Array Product
  }

type State =
  { form          :: AccountForm
  , serverErrors  :: Array (Form.ValidationError NewAccountInputField)
  , user          :: Maybe User
  , newOrder      :: Maybe Order
  , purchaseState :: PurchaseState
  , poller        :: Aff.Fiber Unit
  , isLoading     :: Maybe Spinner.Loading
  , accountStatus :: AccountStatus
  , orderFailure  :: Maybe OrderFailure
  , logger        :: Sentry.Logger
  }

type Self = React.Self Props State

type SetState = (State -> State) -> Effect Unit

type PrevState = { prevProps :: Props, prevState :: State }

data PurchaseState
  = NewPurchase
  | CapturePayment PaymentTerminalUrl
  | ProcessPayment
  | PurchaseFailed
  | PurchaseSetPassword
  | PurchaseCompleted AccountStatus
  | PurchaseSubscriptionExists
  | PurchaseUnexpectedError

data NewAccountInputField
  = EmailAddress
  | ExistingPassword
  | ProductSelection
derive instance eqNewAccountInputField :: Eq NewAccountInputField
instance validatableFieldNewAccountInputField :: Form.ValidatableField NewAccountInputField where
  validateField field value serverErrors = case field of
    EmailAddress     -> Form.validateWithServerErrors serverErrors EmailAddress value Form.validateEmailAddress
    ExistingPassword -> Form.validateEmptyField ExistingPassword "Lösenord krävs." value
    ProductSelection -> Form.validateEmptyField ProductSelection "Produkt krävs." value

type AccountForm =
  { emailAddress     :: Maybe String
  , existingPassword :: Maybe String
  , productSelection :: Maybe Product
  , paymentMethod    :: User.PaymentMethod
  }

data OrderFailure
  = EmailInUse
  | SubscriptionExists
  | FormFieldError (Array NewAccountInputField)
  | AuthenticationError
  | ServerError
  | UnrecognizedError String

type Product =
  { name        :: String
  , id          :: String
  , description :: Array String
  , price       :: Number
  , packageName :: Package.PackageName
  , imageUrl    :: Maybe String -- TODO: What to do with this?
  }

component :: React.Component Props
component = React.createComponent "Vetrina"

app :: Props -> JSX
app = make component
  { initialState: { form:
                      { emailAddress: Nothing
                      , existingPassword: Nothing
                      , productSelection: Nothing
                      , paymentMethod: CreditCard
                      }
                  , serverErrors: []
                  , purchaseState: NewPurchase
                  , user: Nothing
                  , newOrder: Nothing
                  , poller: pure unit
                  , isLoading: Just Spinner.Loading -- Let's show spinner until user logged in
                  , accountStatus: NewAccount
                  , orderFailure: Nothing
                  , logger: Sentry.emptyLogger
                  }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing
  self.setState _ { logger = logger }
  -- Before rendering the form, we need to:
  -- 1. fetch the user if access token is found in the browser
  Aff.launchAff_ do
    Aff.finally
      -- When user has been fetched, hide loading spinner
      (liftEffect $ self.setState \s -> s { isLoading = Nothing })
      do
        -- Try to login with local storage information and set user to state
        User.magicLogin (Just InvalidateCache) $ hush >>> \maybeUser -> self.setState _ { user = maybeUser }

        -- If there is only one product given, automatically select that for the customer
        when (length self.props.products == 1) $
          liftEffect $ self.setState _ { form { productSelection = head self.props.products } }

didUpdate :: Self -> PrevState -> Effect Unit
didUpdate self _ = Aff.launchAff_ $ stopOrderPollerOnCompletedState self

stopOrderPollerOnCompletedState :: Self -> Aff Unit
stopOrderPollerOnCompletedState self =
  case self.state.purchaseState of
    PurchaseFailed      -> killOrderPoller self.state
    PurchaseCompleted _ -> killOrderPoller self.state
    NewPurchase         -> killOrderPoller self.state
    _                   -> pure unit

killOrderPoller :: State -> Aff Unit
killOrderPoller state = Aff.killFiber (error "Canceled poller") state.poller

startOrderPoller :: SetState -> State -> Order -> Effect Unit
startOrderPoller setState state order = do
  newPoller <- Aff.launchAff do
        killOrderPoller state
        newPoller <- Aff.forkAff $ pollOrder setState state (Right order)
        Aff.joinFiber newPoller
  setState _ { poller = newPoller }

pollOrder :: SetState -> State -> Either String Order -> Aff Unit
pollOrder setState state@{ logger } (Right order) = do
  Aff.delay $ Aff.Milliseconds 1000.0
  case order.status.state of
    OrderStarted -> do
      liftEffect $ setState _ { purchaseState = ProcessPayment }
      pollOrder setState state =<< User.getOrder order.number
    OrderCompleted -> do
      let userAccountStatus = maybe NewAccount chooseAccountStatus state.user
          -- If new user, show set password form. Otherwise we're done.
          nextPurchaseStep = case userAccountStatus of
            NewAccount      -> PurchaseSetPassword
            ExistingAccount -> PurchaseCompleted userAccountStatus
      liftEffect $ setState _ { purchaseState = nextPurchaseStep }
      where
        chooseAccountStatus user
          | user.hasCompletedRegistration = ExistingAccount
          | otherwise = NewAccount
    OrderFailed    -> liftEffect do
      logger.error $ Error.orderError "Order failed for customer"
      setState _ { purchaseState = PurchaseFailed }
    OrderCanceled  -> liftEffect do
      logger.log "Customer canceled order" Sentry.Info
      setState _ { purchaseState = NewPurchase }
    OrderCreated   -> pollOrder setState state =<< User.getOrder order.number
    UnknownState   -> liftEffect do
      logger.error $ Error.orderError "Got UnknownState from server"
      setState _ { purchaseState = PurchaseFailed }
pollOrder setState { logger } (Left err) = liftEffect do
  logger.error $ Error.orderError $ "Failed to get order from server: " <> err
  setState _ { purchaseState = PurchaseFailed }

render :: Self -> JSX
render self =
  if isJust self.state.isLoading
  then Spinner.loadingSpinner
  else case self.state.purchaseState of
    NewPurchase -> vetrinaContainer
      [ DOM.h1_ [ title self ]
      , DOM.p_ [ description self ]
      , foldMap orderErrorMessage self.state.orderFailure
      , renderProducts self.props.products
      , accountForm self
          [ maybe (emailAddressInput self) showLoggedInAccount self.state.user
          , case self.state.accountStatus of
              NewAccount      -> acceptTermsCheckbox
              ExistingAccount -> passwordInput self
          , confirmButton self
          , case self.state.accountStatus of
              NewAccount      -> mempty
              ExistingAccount -> resetPasswordLink
          ]
      ]
    CapturePayment url -> vetrinaContainer [ netsTerminalIframe url ]
    ProcessPayment -> Spinner.loadingSpinner
    PurchaseFailed -> DOM.text "PURCHASE FAILED :~("
    PurchaseSetPassword -> vetrinaContainer $ Array.singleton $
      Purchase.SetPassword.setPassword
        -- TODO: The onError callback is invoked if setting the new password fails.
        -- We should think how to handle this. Probably we don't want to
        -- show an ORDER FAILED message, but rather just inform the user that
        -- something went wrong and please try to set the password again some other time.
        { onError: \_ -> pure unit
        , onSuccess: self.setState _ { purchaseState = PurchaseCompleted NewAccount }
        , user: self.state.user
        , logger: self.state.logger
        }
    PurchaseCompleted accountStatus -> vetrinaContainer $ Array.singleton $
      Purchase.Completed.completed
        { onClose: self.props.onClose
        , user: self.state.user
        , accountStatus
        }
    PurchaseSubscriptionExists ->
      DOM.div_
        -- TODO: Waiting for copy
        [ DOM.text "You already have this subscription. Go back to article"
        , DOM.button
            { onClick: handler_ do
                 self.setState _ { purchaseState = NewPurchase }
                 self.props.onClose
            , children: [ DOM.text "OK" ]
            }
        ]
    PurchaseUnexpectedError -> DOM.text "SOMETHING WENT HORRIBLY WRONG SERVER SIDE"

vetrinaContainer :: Array JSX -> JSX
vetrinaContainer children =
  DOM.div
    { className: "vetrina--container"
    , children
    }

renderProducts :: Array Product -> JSX
renderProducts products =
  let descriptions = map ( _.description) products
  in fragment $ map (DOM.p_ <<< Array.singleton <<< intercalate (DOM.br {}) <<< map DOM.text) descriptions

orderErrorMessage :: OrderFailure -> JSX
orderErrorMessage failure =
  case failure of
    AuthenticationError -> InputField.errorMessage "Kombinationen av e-postadress och lösenord finns inte"
    EmailInUse -> mempty -- TODO: Waiting for copy
    _ -> DOM.text "Något gick fel. Vänligen försök om en stund igen."

title :: Self -> JSX
title self@{ state: { accountStatus } } = case accountStatus of
                                            NewAccount ->      DOM.text "Hej kära läsare!"
                                            ExistingAccount -> DOM.text "Du har redan ett KSF Media-konto"

description :: Self -> JSX
description self@{ state: { accountStatus } } = case accountStatus of
                                                  NewAccount ->      DOM.text "Den här artikeln är exklusiv för våra prenumeranter."
                                                  ExistingAccount -> DOM.text "Vänligen logga in med ditt KSF Media lösenord."

resetPasswordLink :: JSX
resetPasswordLink = DOM.p_
                      [ DOM.text "Glömt lösenordet? "
                      , DOM.a
                          { href: "https://www.hbl.fi/losenord/"
                          , children: [ DOM.text "Klicka här"]
                          , target: "_blank"
                          }
                      ]

accountForm :: Self -> Array JSX -> JSX
accountForm self children =
    DOM.form
      { className: "vetrina--form"
      , onSubmit: handler preventDefault $ (\_ -> submitNewOrderForm self $ formValidations self)
      , children
      }

emailAddressInput :: Self -> JSX
emailAddressInput self@{ state: { form, accountStatus }} =
  case accountStatus of
    NewAccount -> DOM.p_ [ DOM.text "Börja med att fylla i din e-post." ]
    ExistingAccount -> mempty
  <> InputField.inputField
  { type_: InputField.Email
  , label: Nothing
  , name: "emailAddress"
  , placeholder: "E-postadress"
  , onChange: (\val -> self.setState _ { form { emailAddress = val
                                              -- If email value is changed, we must consider it as another
                                              -- attempt of creating a new account (it might be that
                                              -- an account with previous email exists, and we are
                                              -- asking the user to log in right now, so changing
                                              -- the email cancels that)
                                              , existingPassword = Nothing
                                              }
                                       -- Look comment about `existingPassword` above ^
                                       , accountStatus = NewAccount
                                       -- Clear server errors of EmailAddress when typing
                                       , serverErrors = Form.removeServerErrors EmailAddress self.state.serverErrors
                                       })
  , validationError: Form.inputFieldErrorMessage $ Form.validateField EmailAddress form.emailAddress self.state.serverErrors
  , value: form.emailAddress
  }

-- TODO: Waiting for copy
showLoggedInAccount :: User.User -> JSX
showLoggedInAccount user = DOM.text $ "Logged in as " <> user.email

-- TODO: Show forgot password link
passwordInput :: Self -> JSX
passwordInput self = InputField.inputField
  { type_: InputField.Password
  , placeholder: "Lösenord"
  , label: Just "Lösenord"
  , name: "accountPassword"
  , value: Nothing
  , onChange: \pw -> self.setState _ { form { existingPassword = pw } }
  , validationError:
      Form.inputFieldErrorMessage $
      Form.validateField ExistingPassword self.state.form.existingPassword []
  }

acceptTermsCheckbox :: JSX
acceptTermsCheckbox =
  let id    = "accept-terms"
      label = """Jag godkänner KSF Medias användarvillkor och
                 bekräftar att jag har läst och förstått integritets-policyn"""
  in DOM.div
    { className: "vetrina--checbox-container"
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
            , children: [ DOM.text label ]
            }
        ]
    }

submitNewOrderForm :: Self -> Form.ValidatedForm NewAccountInputField AccountForm -> Effect Unit
submitNewOrderForm self@{ state: { form, logger } } = unV
  (\errors -> self.setState _ { form { emailAddress = form.emailAddress <|> Just "" } })
  (\validForm -> Aff.launchAff_ $ Spinner.withSpinner (self.setState <<< Spinner.setSpinner) do
      eitherRes <- runExceptT do
        -- If user is found in state, clearly they already have an accout and are logged in
        user <- ExceptT $ case self.state.user of
          Just u  -> pure $ Right u
          Nothing -> case self.state.accountStatus of
            NewAccount      -> createNewAccount self validForm.emailAddress
            ExistingAccount -> loginToExistingAccount self validForm.emailAddress validForm.existingPassword
        ExceptT $ Right unit <$ (liftEffect $ logger.setUser $ Just user)
        product    <- ExceptT $ pure $ note (FormFieldError [ ProductSelection ]) self.state.form.productSelection
        when (userHasPackage product.packageName $ map _.package user.subs)
          $ ExceptT $ pure $ Left SubscriptionExists
        order      <- ExceptT $ createOrder user product
        paymentUrl <- ExceptT $ payOrder order self.state.form.paymentMethod
        pure { paymentUrl, order, user }
      case eitherRes of
        Right { paymentUrl, order, user } ->
          liftEffect do
            let newState = self.state { purchaseState = CapturePayment paymentUrl
                                      , newOrder      = Just order
                                      , user          = Just user
                                      , orderFailure  = Nothing
                                      }
            self.setState \_ -> newState
            -- NOTE: We need to pass the updated state here, not `self.state`.
            startOrderPoller self.setState newState order
        Left err
          | UnrecognizedError e <- err ->
            liftEffect do
              logger.error $ Error.orderError $ "Failed to place an order: " <> e
              self.setState _ { purchaseState = PurchaseFailed }
          | EmailInUse <- err -> liftEffect $ self.setState _ { accountStatus = ExistingAccount, orderFailure = Just EmailInUse }
          | SubscriptionExists <- err -> liftEffect $ self.setState _ { purchaseState = PurchaseSubscriptionExists }
          | otherwise -> liftEffect $ self.setState _ { orderFailure = Just err }
  )

userHasPackage :: PackageName -> Array Package -> Boolean
userHasPackage packageName = isRight <<< Package.findPackage packageName

createNewAccount :: Self -> Maybe String -> Aff (Either OrderFailure User)
createNewAccount self@{ state: { logger } } (Just emailString) = do
  nowISO <- liftEffect $ JSDate.toISOString =<< JSDate.now
  let legalConsent =
        { consentId: "legal_acceptance_v1"
        , screenName: "legalAcceptanceScreen"
        , dateAccepted: nowISO
        }
  newUser <- User.createUserWithEmail { emailAddress: User.Email emailString, legalConsents: [ legalConsent ] }
  case newUser of
    Right user -> pure $ Right user
    Left User.RegistrationEmailInUse -> pure $ Left EmailInUse
    Left (User.InvalidFormFields errors) -> pure $ Left $ UnrecognizedError "invalid form fields"
    _ -> pure $ Left $ UnrecognizedError "Could not create a new account"
createNewAccount _ Nothing = pure $ Left $ UnrecognizedError ""

loginToExistingAccount :: Self -> Maybe String -> Maybe String -> Aff (Either OrderFailure User)
loginToExistingAccount self (Just username) (Just password) = do
  let login = { username, password, mergeToken: toNullable Nothing }
  eitherUser <- User.loginTraditional login
  case eitherUser of
    Right u  -> pure $ Right u
    Left err
      | User.LoginInvalidCredentials <- err -> pure $ Left AuthenticationError
      -- TODO: Think about this
      | User.InvalidFormFields _ <- err -> pure $ Left $ UnrecognizedError "invalid form fields"
      | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
      | User.UnexpectedError jsError <- err -> do
        liftEffect $ self.state.logger.error $ Error.loginError $ message jsError
        pure $ Left $ ServerError
      | otherwise -> pure $ Left $ UnrecognizedError ""
loginToExistingAccount _ _ _ =
  pure $ Left $ FormFieldError [ EmailAddress, ExistingPassword ]

createOrder :: User -> Product -> Aff (Either OrderFailure Order)
createOrder user product = do
  -- TODO: fix period etc.
  let newOrder = { packageId: product.id, period: 1, payAmountCents: ceil $ product.price * 100.0 }
  eitherOrder <- User.createOrder newOrder
  pure $ case eitherOrder of
    Right order -> Right order
    Left err    -> Left $ UnrecognizedError err

payOrder :: Order -> PaymentMethod -> Aff (Either OrderFailure PaymentTerminalUrl)
payOrder order paymentMethod =
  User.payOrder order.number paymentMethod >>= \eitherUrl ->
    pure $ case eitherUrl of
      Right url -> Right url
      Left err  -> Left $ UnrecognizedError err

confirmButton :: Self -> JSX
confirmButton self =
  DOM.input
    { type: "submit"
    , className: "vetrina--button mt2"
    , disabled: isFormInvalid
    , value: "Beställ"
    }
  where
    isFormInvalid
      | Left errs <- toEither $ formValidations self
      = not $ all isNotInitialized errs
      | otherwise = false

formValidations :: Self -> Form.ValidatedForm NewAccountInputField AccountForm
formValidations self@{ state: { form } } =
  { emailAddress: _
  , existingPassword: _
  , productSelection: form.productSelection
  , paymentMethod: form.paymentMethod
  }
  <$> (if isNothing self.state.user
       then Form.validateField EmailAddress form.emailAddress []
       -- If User is already set, we don't care about the email input
       else pure form.emailAddress)
  <*> (case self.state.accountStatus of
            ExistingAccount -> Form.validateField ExistingPassword form.existingPassword []
            -- If NewAccount, we don't need to validate the password field
            NewAccount      -> pure form.existingPassword)

netsTerminalIframe :: PaymentTerminalUrl -> JSX
netsTerminalIframe { paymentTerminalUrl } =
  DOM.iframe
    { src: paymentTerminalUrl
    , className: "vetrina--payment-terminal"
    }
