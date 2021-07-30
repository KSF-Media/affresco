module Prenumerera where

import Prelude

import Bottega as Bottega
import Control.Alt ((<|>))
import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import KSF.Api.Package as Api
import KSF.Paper (Paper(..))
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.Navbar.Component (navbar)
import Prenumerera.Finish as Finish
import Prenumerera.Package (fromApiPackage)
import Prenumerera.PackageSelect as PackageSelect
import Prenumerera.Payment as Payment
import Prenumerera.Register as Register
import Prenumerera.ProgressBar as ProgressBar
import Prenumerera.SelectPeriod as SelectPeriod
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (Match, lit, root, end)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data PrenumereraPage
  = PackageSelectPage
  | CreateAccountPage
  | SelectPeriodPage
  | PaymentPage
  | EndPage

derive instance genericPrenumerera :: Generic PrenumereraPage _

instance showPrenumerera :: Show PrenumereraPage where
  show = genericShow

derive instance eqPrenumerera :: Eq PrenumereraPage

routes :: Match PrenumereraPage
routes =
  (CreateAccountPage <$ (lit "" *> lit "login") <* end) <|>
  (SelectPeriodPage <$ (lit "" *> lit "godkänn") <* end) <|>
  (PaymentPage <$ (lit "" *> lit "betala") <* end) <|>
  (EndPage <$ (lit "" *> lit "bekräftelse") <* end) <|>
  (PackageSelectPage <$ root)

app :: Component {}
app = do
  nav <- makeInterface
  locationState <- nav.locationState
  let initialRoute = either (const $ PackageSelectPage) identity $ match routes locationState.path
      routeListener :: (PrenumereraPage -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
      routeListener setRoute _oldLoc location = do
        case match routes location.pathname of
          Right path -> setRoute path
          Left _ -> pure unit

  packageSelectComponent <- PackageSelect.component
  registerComponent <- Register.component
  selectPeriodComponent <- SelectPeriod.component
  paymentComponent <- Payment.component
  finishComponent <- Finish.component

  component "Prenumerera" \_ -> React.do
    user /\ setUser <- useState Nothing
    loadingUser /\ setLoadingUser <- useState' true
    route /\ setRoute <- useState' initialRoute
    loading /\ setLoading' <- useState' Nothing
    maybePackages /\ setPackages <- useState' Nothing
    brand /\ setBrand <- useState' HBL
    purchasePackage /\ setPurchasePackage <- useState' Nothing
    purchaseDetails /\ setPurchaseDetails <- useState' Nothing
    let startPurchase package description = do
          nav.pushState (unsafeToForeign {}) "/login"
          log $ "start purhcase " <> package.id
          setPurchasePackage $ Just $ Tuple package description
        accountDone = do
          nav.pushState (unsafeToForeign {}) "/godkänn"
          log $ "Account sorted out"
        offerAndMethodSelected offer method = do
          setPurchaseDetails $ Just { offer, method }
          nav.pushState (unsafeToForeign {}) "/betala"
    useEffectOnce do
      let attemptMagicLogin :: Aff.Aff Unit
          attemptMagicLogin =
            User.magicLogin Nothing \userResponse -> do
              case userResponse of
                Right u -> do
--                  Tracking.login (Just user.cusno) "magic login" "success"
                  setUser $ const $ Just u
                _ -> pure unit
              setLoadingUser false
          loadPackages :: Either Error (Array Api.Package) -> Effect Unit
          loadPackages (Right packages) = setPackages $ Just $ mapMaybe fromApiPackage packages
          loadPackages err = do
            log $ "error loading packages " <> show err
            pure unit -- TODO
      Aff.launchAff_ attemptMagicLogin
      Aff.runAff_ loadPackages Bottega.getPackages

      locations (routeListener setRoute) nav

    -- Redirect to main page if we're in funny state
    useEffect route $ do
      case route of
        CreateAccountPage ->
          when (isNothing purchasePackage) $
            nav.pushState (unsafeToForeign {}) "/"
        SelectPeriodPage ->
          when (isNothing purchasePackage) $
            nav.pushState (unsafeToForeign {}) "/"
        PaymentPage -> do
          when (isNothing user || isNothing purchasePackage || isNothing purchaseDetails) $
            nav.pushState (unsafeToForeign {}) "/"
        _ -> pure unit
      pure $ pure unit

    pure $ renderMain brand setUser user nav $
      case loading of
        Nothing -> 
          case route of
            PackageSelectPage ->
              case maybePackages of
                Nothing -> Spinner.loadingSpinner
                Just packages -> packageSelectComponent { packages, startPurchase, setBrand }
            CreateAccountPage ->
              case purchasePackage of
                Nothing -> DOM.text "no package!"
                Just (Tuple package _) ->
                  if loadingUser then Spinner.loadingSpinner else
                    React.fragment
                      [ ProgressBar.render ProgressBar.Login
                      , registerComponent
                          { user
                          , setUser: setUser <<< const
                          , package
                          , withSpinnerUnit: Spinner.withSpinner setLoading'
                          , withSpinnerUser: Spinner.withSpinner setLoading'
                          , next: accountDone
                          , cancel: nav.pushState (unsafeToForeign {}) "/"
                          }
                      ]
            SelectPeriodPage -> fromMaybe mempty do
              Tuple package description <- purchasePackage
              pure $ React.fragment
                [ ProgressBar.render ProgressBar.Accept
                , selectPeriodComponent
                    { package
                    , description
                    , next: offerAndMethodSelected
                    , cancel: nav.pushState (unsafeToForeign {}) "/"
                    }
                ]
            PaymentPage -> fromMaybe mempty do 
              Tuple package description <- purchasePackage
              { offer, method } <- purchaseDetails
              u <- user
              pure $ React.fragment
                [ ProgressBar.render ProgressBar.Payment
                , paymentComponent
                    { user: u
                    , package
                    , description
                    , offer
                    , method
                    , next: nav.pushState (unsafeToForeign {}) "/bekräftelse"
                    }
                ]
            EndPage -> fromMaybe mempty do
              Tuple _ description <- purchasePackage
              { offer, method } <- purchaseDetails
              u <- user
              pure $ React.fragment
                [ ProgressBar.render ProgressBar.Success
                , finishComponent
                    { user: u
                    , description
                    , offer
                    , method
                    }
                ]
        Just Spinner.Loading -> Spinner.loadingSpinner

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

renderMain :: Paper -> ((Maybe User -> Maybe User) -> Effect Unit) -> Maybe User -> PushStateInterface -> JSX -> JSX
renderMain brand setUser user router content =
  React.fragment
    [ navbar { paper: brand
             , adminMode: false
             , isPersonating: false
             , activeUser: user
             , logoutWrapper: Just $
                 \x -> DOM.a
                       { children: [ x ]
                       , onClick: handler_ do
                            router.pushState (unsafeToForeign {}) "/"
                       }
             , logout: setUser $ const Nothing
             }
    , DOM.div
        { className: "ksf-main-container"
        , children: [ content ]
        }
    ]
