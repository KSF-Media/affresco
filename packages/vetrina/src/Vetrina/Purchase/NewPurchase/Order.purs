module Vetrina.Purchase.NewPurchase.Order where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models.Order (OrderSource)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Except.Trans (except)
import Data.Array as Array
import Data.Either (Either(..), either, hush, note)
import Data.Foldable (foldMap, for_)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Nullable (toMaybe, toNullable)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (message)
import KSF.Api.Subscription (Subscription, isSubscriptionCanceled)
import KSF.JSError as Error
import KSF.LocalStorage as LocalStorage
import KSF.Paper (Paper)
import KSF.Sentry as Sentry
import KSF.Spinner as Spinner
import KSF.User (Order, PaymentMethod(..), PaymentTerminalUrl, User)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import Vetrina.Types (AccountStatus(..), FormInputField(..), OrderFailure(..), Product, PurchaseState(..))
import Web.HTML as Web.HTML
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Window
import Web.HTML.Window (Window)

foreign import windowClose :: Window -> Effect Unit

type PurchaseInput r = { productSelection :: Maybe Product, paymentMethod :: Maybe PaymentMethod | r }

type Props =
  { accountStatus         :: AccountStatus
  , products              :: Array Product
  , askAccountAlways      :: Boolean
  , accessEntitlements    :: Set String
  , loading               :: Maybe Spinner.Loading -> Maybe String -> Effect Unit
  , startOrder            :: Maybe PaymentTerminalUrl -> Order -> User -> AccountStatus -> Effect Unit
  , purchaseError         :: Maybe User -> Maybe AccountStatus -> Maybe Product -> PurchaseState -> Effect Unit
  , setRetryPurchase      :: (User -> Effect Unit) -> Effect Unit
  , setUser               :: User -> Effect Unit
  , productSelection      :: Maybe Product
  , onLogin               :: EventHandler
  , headline              :: Maybe JSX
  , paper                 :: Maybe Paper
  , paymentMethod         :: Maybe PaymentMethod -- ^ Pre-selected payment method
  , paymentMethods        :: Array PaymentMethod
  , onPaymentMethodChange :: Maybe PaymentMethod -> Effect Unit
  , onEmailChange         :: Effect Unit
  , customRender          :: Maybe (JSX -> AccountStatus -> JSX)
  , orderSource           :: OrderSource
  }

mkPurchase
  :: forall r
  . Props
  -> Sentry.Logger
  -> Maybe Window
  -> Boolean
  -> PurchaseInput r
  -> Aff (Either OrderFailure User.User)
  -> Effect Unit
mkPurchase props logger window askAccount validForm affUser = do
  Aff.launchAff_ $ Spinner.withSpinner (loadingWithMessage validForm.paymentMethod) do
    eitherUser <- affUser
    either (const $ pure unit) (liftEffect <<< props.setUser) eitherUser
    eitherOrder <- runExceptT do

      -- TODO: We could check for insufficient account before trying to create the order and
      -- wait for the validation done server side. For this, we would need the packages from Bottega so
      -- that we can tell if a package id belongs to a paper product or not.
      -- Insufficient account = user not having contact information and trying to purchase a paper product
      user          <- except eitherUser
      product       <- except $ note (FormFieldError [ ProductSelection ]) validForm.productSelection
      paymentMethod <- except $ note (FormFieldError [ PaymentMethod ])    validForm.paymentMethod

      let existingSubscriptions = Array.filter (\s -> product.id == s.package.id) user.subs
          allCanceled = Array.all isCanceled existingSubscriptions
      when (not allCanceled)
        $ except $ Left SubscriptionExists

      -- Allow new purchases even if entitled if the subscriptions are canceled.
      when (Array.null existingSubscriptions) do
        userEntitlements <- ExceptT getUserEntitlements
        when (isUserEntitled props.accessEntitlements userEntitlements)
          $ except $ Left SubscriptionExists

      -- If props.askAccountAlways is true, mkPurchase gets called once
      -- with askAccount true and this triggers, after which
      -- InsufficientAccount handling will call mkPurchase with
      -- askAccount set as false and the purchase can proceed.
      when askAccount $
        throwError InsufficientAccount

      order <- ExceptT $ createOrder user product props.orderSource
      paymentUrl <- ExceptT $ payOrder order paymentMethod
      liftEffect do
        LocalStorage.setItem "productId" product.id -- for analytics
        LocalStorage.setItem "productPrice" $ show product.priceCents -- for analytics
        LocalStorage.setItem "productCampaingNo" $ foldMap show $ map _.no product.campaign

      pure { user, paymentUrl, order }
    liftEffect $ case eitherOrder of
      Right { user, paymentUrl, order } -> do
        let newAccountStatus = either (const NewAccount) LoggedInAccount eitherUser
        case paymentUrl of
          Just { paymentTerminalUrl } ->
            maybe
            (void <<< Window.open paymentTerminalUrl "_blank" "noopener" =<< Web.HTML.window)
            (Web.HTML.Location.setHref paymentTerminalUrl <=< Window.location) window
          Nothing -> pure unit
        props.startOrder paymentUrl order user newAccountStatus
      Left err -> do
        -- Close the window we might have opened in case we can't get the payment url. We don't have to close
        -- the window in case the further requests fail, Bottega's html responses should handle that.
        for_ window windowClose
        case err of
          UnexpectedError e -> logger.error $ Error.orderError $ "Failed to place an order: " <> e
          _ -> pure unit

        let pError = props.purchaseError (hush eitherUser)
        case err of
          EmailInUse email ->
            pError (Just $ ExistingAccount { email, invalidPassword: false }) validForm.productSelection NewPurchase
          SubscriptionExists ->
            pError Nothing Nothing $ PurchaseFailed SubscriptionExists
          AuthenticationError email -> do
            pError (Just $ ExistingAccount {email, invalidPassword: true}) Nothing $ PurchaseFailed err
          InsufficientAccount -> do
            pError Nothing Nothing $ PurchaseFailed InsufficientAccount
            props.setRetryPurchase $ mkPurchase props logger window true validForm <<< pure <<< Right
          _ ->
            pError Nothing Nothing $ PurchaseFailed $ UnexpectedError ""
  where
    loadingWithMessage paymentMethod spinner =
      props.loading spinner $ case paymentMethod of
        Just CreditCard ->
          if isJust spinner
          then Just "Tack, vi skickar dig nu vidare till betalningsleverantören Nets."
          else Nothing
        _ -> Nothing

isCanceled :: Subscription -> Boolean
isCanceled s = isSubscriptionCanceled s || isJust (toMaybe s.dates.suspend)

isUserEntitled :: Set String -> Set String -> Boolean
isUserEntitled accessEntitlements userEntitlements =
  not $ Set.isEmpty $ Set.intersection accessEntitlements userEntitlements

getUserEntitlements :: Aff (Either OrderFailure (Set String))
getUserEntitlements = do
  eitherEntitlements <- User.getUserEntitlementsLoadToken
  pure case eitherEntitlements of
    Right entitlements -> Right entitlements
    Left err -> Left (UnexpectedError $ show err)

createNewAccount :: Maybe String -> Aff (Either OrderFailure User)
createNewAccount (Just emailString) = do
  nowISO <- liftEffect $ JSDate.toISOString =<< JSDate.now
  let legalConsent =
        { consentId: "legal_acceptance_v1"
        , screenName: "legalAcceptanceScreen"
        , dateAccepted: nowISO
        }
  newUser <- User.createUserWithEmail { emailAddress: User.Email emailString, legalConsents: [ legalConsent ] }
  case newUser of
    Right user -> pure $ Right user
    Left User.RegistrationEmailInUse -> pure $ Left $ EmailInUse emailString
    Left (User.InvalidFormFields _errors) -> pure $ Left $ UnexpectedError "invalid form fields"
    _ -> pure $ Left $ UnexpectedError "Could not create a new account"
createNewAccount Nothing = pure $ Left $ UnexpectedError ""

loginToExistingAccount :: Sentry.Logger -> Maybe String -> Maybe String -> Aff (Either OrderFailure User)
loginToExistingAccount logger (Just username) (Just password) = do
  let login = { username, password, mergeToken: toNullable Nothing }
  eitherUser <- User.loginTraditional login
  case eitherUser of
    Right u  -> pure $ Right u
    Left err
      | User.LoginInvalidCredentials <- err -> pure $ Left $ AuthenticationError username
      -- TODO: Think about this
      | User.InvalidFormFields _ <- err -> pure $ Left $ UnexpectedError "invalid form fields"
      | User.SomethingWentWrong <- err -> pure $ Left $ ServerError
      | User.UnexpectedError jsError <- err -> do
        liftEffect $ logger.error $ Error.loginError $ message jsError
        pure $ Left $ ServerError
      | otherwise -> pure $ Left $ UnexpectedError ""
loginToExistingAccount _ _ _ =
  pure $ Left $ FormFieldError [ EmailAddress, Password ]

createOrder :: User -> Product -> OrderSource -> Aff (Either OrderFailure Order)
createOrder _ product orderSource = do
  -- TODO: fix period etc.
  let newOrder =
        { packageId: product.id
        , period: 1
        , payAmountCents: product.priceCents
        , campaignNo: map _.no product.campaign
        , orderSource: Just orderSource
        }
  eitherOrder <- User.createOrder newOrder
  pure $ case eitherOrder of
    Right order -> Right order
    Left err    -> Left $ toOrderFailure err

payOrder :: Order -> PaymentMethod -> Aff (Either OrderFailure (Maybe PaymentTerminalUrl))
payOrder order paymentMethod =
  User.payOrder order.number paymentMethod >>= \eitherUrl ->
    pure $ case eitherUrl of
      Right url -> Right url
      Left err  -> Left $ toOrderFailure err

toOrderFailure :: BottegaError -> OrderFailure
toOrderFailure bottegaErr =
  case bottegaErr of
    BottegaInsufficientAccount    -> InsufficientAccount
    BottegaTimeout                -> UnexpectedError "Timeout"
    BottegaUnexpectedError errMsg -> UnexpectedError errMsg
