module Prenumerera where

import Prelude

import Bottega as Bottega
import Control.Alt ((<|>))
import Data.Array (elem, filter, mapMaybe)
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import KSF.Api.Package as Api
import KSF.Paper (Paper(..), paperName)
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.Navbar.Component (navbar)
import Prenumerera.Package (PackageId, fromApiPackage)
import Prenumerera.Page.Error as Error
import Prenumerera.Page.Finish as Finish
import Prenumerera.Page.PackageSelect as PackageSelect
import Prenumerera.Page.Payment as Payment
import Prenumerera.Page.Register as Register
import Prenumerera.Page.SelectPeriod as SelectPeriod
import Prenumerera.ProgressBar as ProgressBar
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (Match, lit, root, end)
import Routing.PushState (LocationState, locations, makeInterface)
import Web.HTML (window)
import Web.HTML.Window (scroll)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data PrenumereraPage
  = PackageSelectPage
  | CreateAccountPage
  | SelectPeriodPage
  | PaymentPage
  | EndPage
  | SubscriptionExistsPage
  | PackageLoadFailedPage

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
  (SubscriptionExistsPage <$ (lit "" *> lit "konflikt") <* end) <|>
  (PackageLoadFailedPage <$ (lit "" *> lit "paketfehl") <* end) <|>
  (PackageSelectPage <$ root)

app :: Component {}
app = do
  nav <- makeInterface
  scrollToTop <- map (scroll 0 0) window
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
    route /\ setRoute <- useState' initialRoute
    loading /\ setLoading' <- useState' Nothing
    maybePackages /\ setPackages <- useState' Nothing
    brand /\ setBrand <- useState' HBL
    purchasePackage /\ setPurchasePackage <- useState' Nothing
    purchaseDetails /\ setPurchaseDetails <- useState' Nothing
    let userActivePackages = foldMap getActivePackages user
        startPurchase package description = do
          scrollToTop
          nav.pushState (unsafeToForeign {}) "/login"
          Console.log $ "start purhcase " <> package.id
          setPurchasePackage $ Just $ Tuple package description
        accountDone :: String -> User -> Effect Unit
        accountDone packageId u = do
          scrollToTop
          setUser $ const $ Just u
          nav.pushState (unsafeToForeign {}) $
            if packageId `elem` userActivePackages then "/konflikt" else "/godkänn"
          Console.log $ "Account sorted out"
        offerAndMethodSelected offer method u = do
          scrollToTop
          setPurchaseDetails $ Just { offer, method }
          setUser $ const $ Just u
          nav.pushState (unsafeToForeign {}) "/betala"
        logout = do
          Aff.launchAff_ $ Spinner.withSpinner setLoading' do
            User.logout \logoutResponse -> when (isLeft logoutResponse) $ Console.error "Logout failed"
            liftEffect do
              setUser $ const Nothing
              nav.pushState (unsafeToForeign {}) "/"

    useEffectOnce do
      let attemptMagicLogin :: Aff.Aff Unit
          attemptMagicLogin =
            User.magicLogin Nothing \userResponse -> do
              case userResponse of
                Right u -> do
                  setUser $ const $ Just u
                _ -> pure unit
          loadPackages :: Either Error (Array Api.Package) -> Effect Unit
          loadPackages (Right packages) = setPackages $ Just $ mapMaybe fromApiPackage packages
          loadPackages err = do
            Console.error $ "error loading packages " <> show err
            nav.pushState (unsafeToForeign {}) "/paketfehl"
      Aff.launchAff_ attemptMagicLogin
      Aff.runAff_ loadPackages Bottega.getPackages

      when (route /= PackageSelectPage) $
        nav.pushState (unsafeToForeign {}) "/"

      locations (routeListener setRoute) nav

    -- Redirect to main page if we're in funny state
    useEffect route do
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

    pure $ renderMain brand logout user $
      case loading of
        Nothing ->
          case route of
            PackageSelectPage ->
              case maybePackages of
                Nothing -> Spinner.loadingSpinner
                Just packages ->
                  packageSelectComponent
                    { packages
                    , startPurchase
                    , setBrand
                    , userActivePackages
                    }
            CreateAccountPage ->
              case purchasePackage of
                Nothing -> DOM.text "no package!"
                Just (Tuple package description) ->
                  React.fragment
                    [ ProgressBar.render ProgressBar.Login
                    , registerComponent
                        { user
                        , setUser: setUser <<< const
                        , package
                        , description
                        , scrollToTop
                        , next: accountDone package.id
                        , cancel: nav.pushState (unsafeToForeign {}) "/"
                        }
                    ]
            SelectPeriodPage -> fromMaybe mempty do
              Tuple package description <- purchasePackage
              u <- user
              pure $ React.fragment
                [ ProgressBar.render ProgressBar.Accept
                , selectPeriodComponent
                    { package
                    , description
                    , user: u
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
            SubscriptionExistsPage -> fromMaybe mempty do
              pure $ React.fragment
                [ ProgressBar.render ProgressBar.Login
                , Error.renderSubscriptionExists
                ]
            PackageLoadFailedPage ->
              Error.renderPackageLoadFailed
        Just Spinner.Loading -> Spinner.loadingSpinner
  where
    -- Given a User, find which uncancelled packages they have.  Making
    -- such an order would fail.
    getActivePackages :: User -> Array PackageId
    getActivePackages user =
      map (_.id <<< _.package) $
      filter (\{dates} -> isNothing $ Nullable.toMaybe dates.suspend) user.subs

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

renderMain :: Paper -> Effect Unit -> Maybe User -> JSX -> JSX
renderMain brand logout user content =
  navbar
    { paper: brand
    , activeUser: user
    , specialHelp: Nothing
    , logout
    } <>
  DOM.div
    { className: "ksf-main-container"
    , children: [ content ]
    } <>
  DOM.footer
    { className: "ksf-block-footer"
    , children:
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.ul
                    { id: "footer-links"
                    , children:
                        [ DOM.li_
                            [ DOM.strong_ [ DOM.text "Kundservice" ]
                            , link "https://www.hbl.fi/kundservice/" HBL
                            , link "https://www.vastranyland.fi/kundservice/" VN
                            , link "https://www.ostnyland.fi/kundservice/" ON
                            ]
                        , DOM.li_
                            [ DOM.strong_ [ DOM.text "Ta kontakt" ]
                            , link "https://www.hbl.fi/kontakt/" HBL
                            , link "https://www.vastranyland.fi/kontakt/" VN
                            , link "https://www.ostnyland.fi/kontakt/" ON
                            ]
                        ]
                    }
                , DOM.div
                    { id: "brands"
                    , children:
                        [ imgLink "https://www.hbl.fi/" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/hbl.png" HBL
                        , imgLink "https://www.vastranyland.fi/" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/vn.png" VN
                        , imgLink "https://www.ostnyland.fi/" "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/on.png" ON
                        ]
                    }
                , DOM.div
                    { id: "ksf"
                    , children:
                        [ DOM.div
                            { id: "ksf-infos"
                            , children:
                                [ DOM.text "KSF Media "
                                , separator
                                , DOM.text "Mannerheimvägen 18"
                                , separator
                                , DOM.text "001000 Helsingfors"
                                ]
                            }
                        , DOM.img
                            { src: "https://cdn.ksfmedia.fi/prenumerera.ksfmedia.fi/images/ksf-media.png"
                            , alt: "KSF Media"
                            }
                        ]
                    }
                ]
            }
        ]
    }
  where
    link href paper = DOM.a { href, children: [ DOM.text $ paperName paper ] }
    imgLink href src paper =
      DOM.a
        { href
        , children: [ DOM.img { src, alt: paperName paper } ]
        }
    separator = DOM.span { className: "separator", children: [ DOM.text "–" ] }
