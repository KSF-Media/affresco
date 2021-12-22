module MittKonto.Main.UserView.Subscription.Elements where

import Prelude

import Data.Array (filter, find, mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.DateTime (date)
import Data.Enum (enumFromTo)
import Data.Foldable (foldMap, for_, null, maximum)
import Data.JSDate (toDate)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String (length, splitAt, trim)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import KSF.Api.Subscription (PendingAddressChange(..), PausedSubscription(..), SubscriptionPaymentMethod(..), SubscriptionDates(..), Subscription (..), isSubscriptionPausable, isSubscriptionTemporaryAddressChangable)
import KSF.Api.Subscription (toString) as Subsno
import KSF.Api.Package (Package (..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DeliveryReclamation as DeliveryReclamation
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.Helpers (formatDateDots)
import KSF.JSError as Error
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.Sentry as Sentry
import KSF.TemporaryAddressChange.Component as TemporaryAddressChange
import KSF.Tracking as Tracking
import KSF.User (InvalidDateInput(..))
import KSF.User as User
import MittKonto.Main.UserView.Subscription.Helpers as Helpers
import MittKonto.Main.UserView.Subscription.Types as Types
import MittKonto.Wrappers.ActionsWrapper (actionsWrapper) as ActionsWrapper
import MittKonto.Wrappers.Elements (successWrapper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events (handler, handler_)

receiverName :: Types.Self -> Array DescriptionList.Definition
receiverName { props: { subscription: Subscription { receiver } } } =
  foldMap (\r -> Array.singleton
              { term: "Mottagare:"
              , description: [ DOM.text r ]
              }) receiver

deliveryAddress :: Types.Self -> Array DescriptionList.Definition
deliveryAddress { props: { subscription: Subscription { deliveryAddress: subDeliveryAddress, package: Package package }, user: { address: userAddress } } } =
  if package.digitalOnly
  then mempty
  else Array.singleton
         { term: "Leveransadress:"
         , description: [ DOM.text currentDeliveryAddress ]
         }
  where
    currentDeliveryAddress :: String
    currentDeliveryAddress
      | Just address <- subDeliveryAddress
      = Helpers.formatAddress address
      | Just { streetAddress, zipCode, city } <- toMaybe userAddress
      = intercalate ", "
          [ streetAddress
          , fromMaybe "-" $ toMaybe zipCode
          , fromMaybe "-" $ toMaybe city
          ]
      | otherwise = "-"

paymentMethod :: Types.Self -> Array DescriptionList.Definition
paymentMethod { props: { subscription: Subscription { paymentMethod: method, paymentMethodId }, user: { creditCards } } } = Array.singleton
  { term: "Faktureringsmetod:"
  , description: [ DOM.div_ [ DOM.text $ Helpers.translatePaymentMethod method
                            , case method of
                                CreditCard -> subscriptionCreditCard
                                _ -> mempty
                            ]
                 ]
  }
  where
    subscriptionCreditCard :: JSX
    subscriptionCreditCard
      | Just id <- paymentMethodId,
        Just card <- find (\c -> c.paymentMethodId == id) creditCards =
          DOM.ul_ [ DOM.li_ [ DOM.text $ "Nummer: " <> card.maskedPan ]
                  , DOM.li_ [ DOM.text $ "Utgångsdatum: " <> formatExpiryDate card.expiryDate ]
                  ]
      | otherwise = mempty

    formatExpiryDate :: String -> String
    formatExpiryDate expiryDate
      | (length expiryDate) == 4 =
          let { before: year, after: month } = splitAt 2 expiryDate
           in
            month <> "/" <> year
      | otherwise = ""

pendingAddressChanges :: Types.Self -> Array DescriptionList.Definition
pendingAddressChanges self@{ state: { pendingAddressChanges: pendingChanges }, props: { now } } =
  if Array.null filteredChanges then mempty else Array.singleton $
  { term: "Tillfällig adressändring:"
  , description: map (showPendingAddressChange self) $
      Array.zip (enumFromTo 1 $ Array.length filteredChanges) filteredChanges
  }
  where
    filteredChanges = foldMap filterExpiredPendingChanges pendingChanges
    filterExpiredPendingChanges :: Array User.PendingAddressChange -> Array User.PendingAddressChange
    filterExpiredPendingChanges changes =
      filter (not <<< Helpers.isPeriodExpired true now <<< _.endDate <<< unwrap) changes

showPendingAddressChange :: Types.Self -> (Tuple Int User.PendingAddressChange) -> JSX
showPendingAddressChange self (Tuple n change@(PendingAddressChange { address, startDate, endDate })) =
  let addressString = Helpers.formatAddress address
      pendingPeriod = Helpers.formatDateString startDate endDate
  in DOM.div
       { children: [ changeButton self
                       (Types.EditTemporaryAddressChange change)
                       (temporaryAddressChangeComponent self $ Just change)
                   , DOM.text $ addressString <> " (" <> pendingPeriod <> ")"
                   ]
       , id: "subscription-" <> Subsno.toString (_.subsno $ unwrap self.props.subscription) <> "-pending-address-change-" <> show n
       }

billingDateTerm :: Types.Self -> Array DescriptionList.Definition
billingDateTerm { props: { subscription: Subscription { dates: SubscriptionDates { end } } } } = foldMap
  (\e -> Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text $ e ]
      }
  ) $ map (trim <<< formatDateDots <<< date) end

subscriptionEndTerm :: Types.Self -> Array DescriptionList.Definition
subscriptionEndTerm { props: { subscription: Subscription { dates: SubscriptionDates { suspend } } } } = foldMap
  (\s -> Array.singleton $
      { term: "Prenumerationens slutdatum:"
      , description: [ DOM.text s ]
      }
  ) $ map (trim <<< formatDateDots <<< date) suspend

subscriptionUpdates :: Types.Self -> JSX
subscriptionUpdates self@{ props: props@{ now, subscription: Subscription sub@{ subsno, package: Package package } }, state } =
  Grid.row_ [ actionsWrapper ]
  where
    actionsWrapper = ActionsWrapper.actionsWrapper
      { actions: (if package.digitalOnly then
                   mempty
                 else
                   paperOnlyActions)
                 <> extraActions
      , wrapperState: self.state.wrapperProgress
      , onTryAgain: self.setState _ { wrapperProgress = updateProgress }
      , containerClass: "subscription--actions-container flex"
      }

    paperOnlyActions =
      [ if isSubscriptionPausable (Subscription sub) then pauseIcon else mempty
      , if maybe true (Array.null <<< filter (not <<< Helpers.isPeriodExpired true now <<< _.endDate <<< unwrap))
           self.state.pausedSubscriptions
          then mempty
          else removeSubscriptionPauses
      , if isSubscriptionTemporaryAddressChangable (Subscription sub) then temporaryAddressChangeIcon else mempty
      , case self.state.pendingAddressChanges of
              Just a | not $ null $ filter (not <<< Helpers.isPeriodExpired false now <<< _.endDate <<< unwrap) a ->
                removeTempAddressChanges a
              _ -> mempty
      , deliveryReclamationIcon
      ]

    extraActions =
      if sub.paymentMethod == CreditCard && sub.paycusno == props.user.cusno
        then [ creditCardUpdateIcon ]
        else mempty

    updateProgress =
      case state.updateAction of
        Just Types.PauseSubscription      -> AsyncWrapper.Editing $ pauseSubscriptionComponent self Nothing
        Just (Types.EditSubscriptionPause pause) ->
          AsyncWrapper.Editing $ pauseSubscriptionComponent self $ Just pause
        Just Types.TemporaryAddressChange -> AsyncWrapper.Editing $ temporaryAddressChangeComponent self Nothing
        Just (Types.EditTemporaryAddressChange change) ->
          AsyncWrapper.Editing $ temporaryAddressChangeComponent self $ Just change
        Just Types.DeliveryReclamation    -> AsyncWrapper.Editing deliveryReclamationComponent
        Nothing                           -> AsyncWrapper.Ready

    deliveryReclamationComponent =
      DeliveryReclamation.deliveryReclamation
        { subsno:   _.subsno $ unwrap props.subscription
        , cusno:    props.user.cusno
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \_ ->
                        self.setState _
                            { wrapperProgress = AsyncWrapper.Success $ Just $ successWrapper Nothing Helpers.successText
                            }
        , onError: \err -> do
            self.props.logger.error $ Error.subscriptionError Error.SubscriptionReclamation $ show err
            self.setState _ { wrapperProgress = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }
        }

    pauseIcon =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--pause-icon circle"
                , onClick: showPauseView
                }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Gör uppehåll för papperstidningen" ] ]
                , onClick: showPauseView
                }
            ]
        }
        where
          showPauseView = handler_ $
            self.setState _
            { updateAction = Just Types.PauseSubscription
            , wrapperProgress = AsyncWrapper.Editing $ pauseSubscriptionComponent self Nothing
            }

    removeSubscriptionPauses =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--unpause-icon circle" }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Ta bort alla uppehåll" ] ]
                }
            ]
        , onClick: handler_ $ do
            self.setState _
              { wrapperProgress = AsyncWrapper.Loading mempty }
            Aff.launchAff_ $ do
              unpausedSubscription <- Aff.try $ do
                User.unpauseSubscription props.user.uuid (_.subsno $ unwrap props.subscription)
              case unpausedSubscription of
                Left _ -> liftEffect do
                  self.setState _
                    { wrapperProgress = AsyncWrapper.Error "Uppehållet kunde inte tas bort. Vänligen kontakta kundtjänst." }
                  Tracking.unpauseSubscription props.user.cusno (_.subsno $ unwrap props.subscription) "error"
                Right (Subscription newSubscription) -> liftEffect do
                  self.setState _
                    { pausedSubscriptions = newSubscription.paused
                    , wrapperProgress = AsyncWrapper.Success $ Just $ successWrapper Nothing "Uppehållet har tagits bort"
                    }
                  Tracking.unpauseSubscription props.user.cusno newSubscription.subsno "success"
        }

    temporaryAddressChangeIcon =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--temporary-address-change-icon circle"
                , onClick: showTemporaryAddressChange
                }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Gör tillfällig adressändring" ] ]
                , onClick: showTemporaryAddressChange
                }
            ]
        }
        where
            showTemporaryAddressChange = handler_ $ do
              self.setState _
                { updateAction = Just Types.TemporaryAddressChange
                , wrapperProgress = AsyncWrapper.Editing $ temporaryAddressChangeComponent self Nothing
                }

    removeTempAddressChanges tempAddressChanges =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--delete-temporary-address-change-icon circle" }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Avbryt tillfälliga adressändringar" ] ]
                }
            ]
        , onClick: handler_ $ do
            self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
            Aff.launchAff_ $ do
                for_ tempAddressChanges $ \(PendingAddressChange tempAddressChange) -> do
                  let startDate = date tempAddressChange.startDate
                      endDate   = map date tempAddressChange.endDate
                      sendAnalyticsEvent start end status =
                        Tracking.deleteTempAddressChange (_.cusno $ unwrap props.subscription) (_.subsno $ unwrap props.subscription) start end status
                  case startDate, endDate of
                    startDate', endDate' -> do
                      tempAddressChangesDeleted <- User.deleteTemporaryAddressChange props.user.uuid (_.subsno $ unwrap props.subscription) startDate' endDate'
                      case tempAddressChangesDeleted of
                        Right (Subscription newSubscription) -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Success $ Just $ successWrapper Nothing "Tillfällig adressändring har tagits bort",
                            pendingAddressChanges = newSubscription.pendingAddressChanges }
                          sendAnalyticsEvent startDate' endDate' "success"
                        Left _ -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Error "Tillfälliga addressförändringar kunde inte tas bort. Vänligen kontakta kundtjänst." }
                          sendAnalyticsEvent startDate' endDate' "error"

        }

    deliveryReclamationIcon =
      DOM.div
        { className: "subscription--action-item"
        , children:
            [ DOM.div
                { className: "subscription--delivery-reclamation-icon circle"
                , onClick: showDeliveryReclamation
                }
            , DOM.span
                { className: "subscription--update-action-text"
                , children:
                    [ DOM.u_ [ DOM.text "Reklamation av utebliven tidning" ] ]
                , onClick: showDeliveryReclamation
                }
            ]
        }
      where
        showDeliveryReclamation = handler_ $ do
          self.setState _
            { updateAction = Just Types.DeliveryReclamation
            , wrapperProgress = AsyncWrapper.Editing deliveryReclamationComponent
            }

    creditCardUpdateIcon =
      DOM.div
        { className: "subscription--action-item"
        , children: [ DOM.a
                        { onClick: handler preventDefault $ const $ props.router.pushState (unsafeToForeign {}) $
                                   "/prenumerationer/" <> Subsno.toString subsno <> "/kreditkort/uppdatera"
                        , href: "/prenumerationer/" <> Subsno.toString subsno <> "/kreditkort/uppdatera"
                        , children: [ DOM.div
                                        { className: "subscription--action-link"
                                        , children: [ DOM.div
                                                        { className: "subscription--credit-card-update-icon circle"
                                                        }
                                                    , DOM.span
                                                        { className: "subscription--update-action-text"
                                                        , children:
                                                            [ DOM.u_ [ DOM.text "Uppdatera ditt kredit- eller bankkort" ] ]
                                                        }
                                                    ]
                                        }
                                    ]
                        }
                    ]
        }

pauseSubscriptionComponent :: Types.Self -> Maybe User.PausedSubscription -> JSX
pauseSubscriptionComponent self@{ props: props@{ subscription: Subscription sub@{ package: Package package } } } editing =
  PauseSubscription.pauseSubscription
    { subsno: sub.subsno
    , cusno: props.user.cusno
    , userUuid: props.user.uuid
      -- Make sure that both exist if used
    , oldStart: oldEnd *> oldStart
    , oldEnd: oldStart *> oldEnd
    , nextDelivery: map date package.nextDelivery
    , lastDelivery: maximum $ mapMaybe (map date <<< _.nextDelivery <<< unwrap) package.products
    , now: self.props.now
    , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
    , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
    , onSuccess: \(Subscription pausedSubscription) ->
                    self.setState _
                      { pausedSubscriptions = pausedSubscription.paused
                      , wrapperProgress = AsyncWrapper.Success $ Just $ successWrapper pauseReadMsg Helpers.successText
                      }
    , onError: \err -> do
        let unexpectedError = "Något gick fel och vi kunde tyvärr inte genomföra den aktivitet du försökte utföra. Vänligen kontakta vår kundtjänst."
            startDateError = "Din begäran om uppehåll i beställningen misslyckades. Uppehåll kan endast påbörjas fr.o.m. följande dag."
            lengthError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehålls perioden är för kort eller lång. Uppehållsperioden bör vara mellan 7 dagar och 3 månader långt."
            overlappingError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet går över ett annat uppehåll. Det måste vara minst en vecka mellan uppehållsperioderna."
            tooRecentError = "Din begäran om uppehåll i beställningen misslyckades, eftersom uppehållet är för nära en annan uppehållsperiod. Det måste vara minst en vecka mellan uppehållsperioderna."
            errMsg = case err of
              InvalidStartDate   -> startDateError
              InvalidLength      -> lengthError
              InvalidOverlapping -> overlappingError
              InvalidTooRecent   -> tooRecentError
              InvalidUnexpected  -> unexpectedError
        case err of
          InvalidUnexpected ->
            self.props.logger.error
            $ Error.subscriptionError
              (if isNothing (oldStart *> oldEnd)
                 then Error.SubscriptionPause
                 else Error.EditSubscriptionPause)
            $ show err
              -- Other cases are not really errors we want notifications from
          _ -> self.props.logger.log (show err) Sentry.Info
        self.setState _ { wrapperProgress = AsyncWrapper.Error errMsg }
    }
  where
    oldStart = (date <<< _.startDate <<< unwrap) <$> editing
    oldEnd = map date (_.endDate <<< unwrap =<< editing)
    pauseReadMsg = Just $
      DOM.div
        { children: [ DOM.text "Du kan läsa tidningen digitalt utan extra kostnad under uppehållet." ]
        }

temporaryAddressChangeComponent :: Types.Self -> Maybe User.PendingAddressChange -> JSX
temporaryAddressChangeComponent self@{ props: props@{ subscription: Subscription sub@{ package: Package package } } } editing =
  TemporaryAddressChange.temporaryAddressChange
    { subsno: sub.subsno
    , cusno: props.user.cusno
    , pastAddresses: readPastTemporaryAddress <$> props.user.pastTemporaryAddresses
    , nextDelivery: map date package.nextDelivery
    , lastDelivery: maximum $ mapMaybe (map date <<< _.nextDelivery <<< unwrap) package.products
    , editing: editing
    , userUuid: props.user.uuid
    , now: self.props.now
    , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
    , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
    , onSuccess: \(Subscription { pendingAddressChanges: newPendingChanges }) ->
                    self.setState _
                        { pendingAddressChanges = newPendingChanges
                        , wrapperProgress = AsyncWrapper.Success $ Just $ successWrapper Nothing Helpers.successText
                        }

    , onError: \(err :: User.InvalidDateInput) -> do
        let unexpectedError = "Något gick fel och vi kunde tyvärr inte genomföra den aktivitet du försökte utföra. Vänligen kontakta vår kundtjänst."
            startDateError = "Din begäran om tillfällig adressändring i beställningen misslyckades. Tillfällig adressändring kan endast påbörjas fr.o.m. följande dag."
            lengthError = "Din begäran om tillfällig adressändring i beställningen misslyckades, eftersom tillfällig adressändring perioden är för kort. Adressändringperioden bör vara åtminstone 7 dagar långt."
            errMsg = case err of
              InvalidStartDate   -> startDateError
              InvalidLength      -> lengthError
              _                  -> unexpectedError
        case err of
          InvalidUnexpected ->
            self.props.logger.error
            $ Error.subscriptionError Error.SubscriptionTemporaryAddressChange
            $ show err
          -- Other cases are not really errors we want notifications from
          _ -> self.props.logger.log (show err) Sentry.Info
        self.setState _ { wrapperProgress = AsyncWrapper.Error errMsg }
    }
  where
    readPastTemporaryAddress tmp =
      { streetAddress: Just tmp.street
      , zipCode: Just tmp.zipcode
      , cityName: toMaybe tmp.cityName
      , countryCode: Just tmp.countryCode
      , temporaryName: toMaybe tmp.temporaryName
      }

showPausedDates :: Types.Self -> Array User.PausedSubscription -> Array JSX
showPausedDates self =
  let formatDates (PausedSubscription { startDate, endDate }) = Helpers.formatDateString startDate endDate
      pauseLine :: User.PausedSubscription -> JSX
      pauseLine pause =
        let text = "Uppehåll: " <> formatDates pause
        in case map date (_.endDate $ unwrap pause) of
          Just _endDate ->
            DOM.div
              { children: [ changeButton self
                              (Types.EditSubscriptionPause pause)
                              (pauseSubscriptionComponent self $ Just pause)
                          , DOM.text text
                          ]
              , className: "subscription--subscription-pause"
              }
          _ -> DOM.text text
  in map pauseLine

changeButton :: Types.Self -> Types.SubscriptionUpdateAction -> JSX -> JSX
changeButton self updateAction component =
  DOM.div
    { className: "subscription--edit-period-button"
    , children:
        [ DOM.div
            { className: "edit-icon circle"
            , onClick: capture_ startEdit
            }
        , DOM.span
            { className: "subscription--edit-text"
            , onClick: capture_ startEdit
            , children: [ DOM.text "Ändra" ]
            }
        ]
    }
  where
    startEdit = self.setState _
                  { updateAction = Just updateAction
                  , wrapperProgress = AsyncWrapper.Editing component
                  }
