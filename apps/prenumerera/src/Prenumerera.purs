module Prenumerera where

import Prelude

import Bottega as Bottega
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Unsafe (unsafePerformEffect)
import KSF.Api.Package (Package)
import KSF.Paper (Paper(HBL))
import KSF.Spinner as Spinner
import KSF.User (User)
import KSF.User as User
import KSF.Navbar.Component (navbar)
import Prenumerera.ProductSelect as ProductSelect
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.Match (Match, lit, root, end, param)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (write)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Debug

data PrenumereraPage
  = ProductSelectPage
  | CreateAccountPage
  | SelectPeriodPage
  | EndPage String String

derive instance genericMyADT :: Generic PrenumereraPage _

instance showMyADT :: Show PrenumereraPage where
  show = genericShow

routes :: Match PrenumereraPage
routes =
  (CreateAccountPage <$ (lit "" *> lit "login") <* end) <|>
  (SelectPeriodPage <$ (lit "" *> lit "godkänn") <* end) <|>
  (EndPage <$ (lit "" *> lit "nets" *> lit "return") <*> param "transactionId" <*> param "responseCode" <* end) <|>
  (ProductSelectPage <$ root)

app :: Component {}
app = do
  nav <- makeInterface
  locationState <- nav.locationState
  let initialRoute = either (const $ ProductSelectPage) identity $ match routes locationState.path
      routeListener :: (PrenumereraPage -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
      routeListener setRoute _oldLoc location = do
        case match routes location.pathname of
          Right path -> setRoute path
          Left _ -> pure unit
      startPurchase package = do
        log $ "start purhcase " <> package.id

  productSelectComponent <- ProductSelect.component

  component "Prenumerera" \_ -> React.do
    user /\ setUser <- useState Nothing
    route /\ setRoute <- useState' initialRoute
--    loading /\ setLoading <- useState' false
    maybePackages /\ setPackages <- useState' Nothing
    useEffectOnce do
      let attemptMagicLogin :: Aff.Aff Unit
          attemptMagicLogin =
            User.magicLogin Nothing \userResponse ->
              case userResponse of
                Right u -> do
--                  Tracking.login (Just user.cusno) "magic login" "success"
                  setUser $ const $ Just u
                _ -> pure unit
      --Aff.runAff_ $ Spinner.withSpinner (setLoading <<< isJust) attemptMagicLogin
--      Aff.runAff_ do
          loadPackages :: Either Error (Array Package) -> Effect Unit
          loadPackages (Right packages) = setPackages $ Just packages
          loadPackages err = do
            log $ "error loading packages " <> show err
            pure unit -- TODO
--      Aff.launchAff_ attemptMagicLogin
      Aff.runAff_ loadPackages Bottega.getPackages

      locations (routeListener setRoute) nav

    pure $ renderMain setUser user nav $
      trace ("route " <> show route) $ \_ -> case route of
        ProductSelectPage ->
          case maybePackages of
            Nothing -> Spinner.loadingSpinner
            Just packages -> productSelectComponent { user, nav, packages, startPurchase }
        _ -> mempty

jsApp :: {} -> JSX
jsApp = unsafePerformEffect app

renderMain :: ((Maybe User -> Maybe User) -> Effect Unit) -> Maybe User -> PushStateInterface -> JSX -> JSX
renderMain setUser user router content =
  DOM.div
    { className: "prenumerera"
    , children:
        [ navbar { paper: HBL
                 , adminMode: false
                 , isPersonating: false
                 , activeUser: user
                 , logoutWrapper: Just $
                     \x -> DOM.a
                             { children: [ x ]
                             , onClick: handler_ do
                                  pure unit
                                  router.pushState (write {}) "/"
                             }
                 , logout: setUser $ const Nothing
                 }
        , DOM.div
            { className: "mt3 mb4 clearfix"
            , children:
                [ DOM.div
                    { className: "prenumerera--main-container"
                    , children: [ content ]
                    }
                ]
            }
        ]
    }
