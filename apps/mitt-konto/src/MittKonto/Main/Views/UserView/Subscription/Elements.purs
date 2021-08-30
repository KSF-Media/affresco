module MittKonto.Main.UserView.Subscription.Elements where

import Prelude

import Data.Array (filter, find, mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (foldMap, for_, null, maximum)
import Data.JSDate (toDate)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String (length, splitAt, trim)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..), isSubscriptionPausable, isSubscriptionTemporaryAddressChangable)
import KSF.Api.Subscription (toString) as Subsno
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
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events (handler, handler_)

receiverName :: Types.Self -> Array DescriptionList.Definition
receiverName { props: { subscription: { receiver } } } =
  foldMap (\r -> Array.singleton
              { term: "Mottagare:"
              , description: [ DOM.text r ]
              }) $ toMaybe receiver

deliveryAddress :: Types.Self -> Array DescriptionList.Definition
deliveryAddress { props: { subscription: { deliveryAddress: subDeliveryAddress, package }, user: { address: userAddress } } } =
  if package.digitalOnly
  then mempty
  else Array.singleton
         { term: "Leveransadress:"
         , description: [ DOM.text currentDeliveryAddress ]
         }
  where
    currentDeliveryAddress :: String
    currentDeliveryAddress
      | Just address <- toMaybe subDeliveryAddress
      = Helpers.formatAddress address
      | Just { streetAddress, zipCode, city } <- toMaybe userAddress
      = intercalate ", "
          [ streetAddress
          , fromMaybe "-" $ toMaybe zipCode
          , fromMaybe "-" $ toMaybe city
          ]
      | otherwise = "-"

paymentMethod :: Types.Self -> Array DescriptionList.Definition
paymentMethod { props: { subscription: { paymentMethod: method, paymentMethodId }, user: { creditCards } } } = Array.singleton
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
      | Just id <- toMaybe paymentMethodId,
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
      filter (not <<< Helpers.isPeriodExpired true now <<< toMaybe <<< _.endDate) changes

showPendingAddressChange :: Types.Self -> (Tuple Int User.PendingAddressChange) -> JSX
showPendingAddressChange self (Tuple n change@{ address, startDate, endDate }) =
  let addressString = Helpers.formatAddress address
      pendingPeriod = Helpers.formatDateString startDate (toMaybe endDate)
  in DOM.div
       { children: [ changeButton self
                       (Types.EditTemporaryAddressChange change)
                       (temporaryAddressChangeComponent self $ Just change)
                   , DOM.text $ addressString <> " (" <> pendingPeriod <> ")"
                   ]
       , id: "subscription-" <> Subsno.toString self.props.subscription.subsno <> "-pending-address-change-" <> show n
       }

billingDateTerm :: Types.Self -> Array DescriptionList.Definition
billingDateTerm { props: { subscription: { dates: { end } } } } = foldMap
  (\e -> Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text $ e ]
      }
  ) $ trim <<< formatDateDots <$> (toDate =<< toMaybe end)

subscriptionEndTerm :: Types.Self -> Array DescriptionList.Definition
subscriptionEndTerm { props: { subscription: { dates: { suspend } } } } = foldMap
  (\s -> Array.singleton $
      { term: "Prenumerationens slutdatum:"
      , description: [ DOM.text s ]
      }
  ) $ trim <<< formatDateDots <$> (toDate =<< toMaybe suspend)

subscriptionUpdates :: Types.Self -> JSX
subscriptionUpdates self@{ props: props@{ now, subscription: sub@{ subsno, package } }, state } =
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
      [ if isSubscriptionPausable sub then pauseIcon else mempty
      , if maybe true (Array.null <<< filter (not <<< Helpers.isPeriodExpired true now <<< toMaybe <<< _.endDate))
           self.state.pausedSubscriptions
          then mempty
          else removeSubscriptionPauses
      , if isSubscriptionTemporaryAddressChangable sub then temporaryAddressChangeIcon else mempty
      , case self.state.pendingAddressChanges of
              Just a | not $ null $ filter (not <<< Helpers.isPeriodExpired false now <<< toMaybe <<< _.endDate) a ->
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
        { subsno:   props.subscription.subsno
        , cusno:    props.user.cusno
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \_ ->
                        self.setState _
                            { wrapperProgress = AsyncWrapper.Success Helpers.successText
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
                    [ DOM.u_ [ DOM.text "Gör uppehåll" ] ]
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
                User.unpauseSubscription props.user.uuid props.subscription.subsno
              case unpausedSubscription of
                Left _ -> liftEffect do
                  self.setState _
                    { wrapperProgress = AsyncWrapper.Error "Uppehållet kunde inte tas bort. Vänligen kontakta kundtjänst." }
                  Tracking.unpauseSubscription props.user.cusno props.subscription.subsno "error"
                Right newSubscription -> liftEffect do
                  self.setState _
                    { pausedSubscriptions = toMaybe newSubscription.paused
                    , wrapperProgress = AsyncWrapper.Success $ Just "Uppehållet har tagits bort"
                    }
                  Tracking.unpauseSubscription props.user.cusno props.subscription.subsno "success"
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
                for_ tempAddressChanges $ \tempAddressChange -> do
                  let startDate = toDate tempAddressChange.startDate
                  let endDate   = toDate =<< toMaybe tempAddressChange.endDate
                  case startDate, endDate of
                    (Just startDate'), endDate' -> do
                      tempAddressChangesDeleted <- User.deleteTemporaryAddressChange props.user.uuid props.subscription.subsno startDate' endDate'
                      case tempAddressChangesDeleted of
                        Right newSubscription -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Success $ Just "Tillfällig adressändring har tagits bort",
                            pendingAddressChanges = toMaybe newSubscription.pendingAddressChanges }
                          Tracking.deleteTempAddressChange props.subscription.cusno props.subscription.subsno startDate' endDate' "success"
                        Left _ -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Error "Tillfälliga addressförändringar kunde inte tas bort. Vänligen kontakta kundtjänst." }
                          Tracking.deleteTempAddressChange props.subscription.cusno props.subscription.subsno startDate' endDate' "success"
                    _, _ -> liftEffect $ self.setState _ { wrapperProgress = AsyncWrapper.Error "Tillfällig addressändring kunde inte tas bort. Vänligen kontakta kundtjänst." }
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
pauseSubscriptionComponent self@{ props: props@{ subscription: sub@{ package } } } editing =
  PauseSubscription.pauseSubscription
    { subsno: sub.subsno
    , cusno: props.user.cusno
    , userUuid: props.user.uuid
      -- Make sure that both exist if used
    , oldStart: oldEnd *> oldStart
    , oldEnd: oldStart *> oldEnd
    , nextDelivery: toDate =<< toMaybe package.nextDelivery
    , lastDelivery: maximum $ mapMaybe (toDate <=< toMaybe <<< _.nextDelivery) package.products
    , now: self.props.now
    , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
    , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
    , onSuccess: \pausedSubscription ->
                    self.setState _
                      { pausedSubscriptions = toMaybe pausedSubscription.paused
                      , wrapperProgress = AsyncWrapper.Success Helpers.successText
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
    oldStart = (toDate <<< _.startDate) =<< editing
    oldEnd = ((toDate <=< toMaybe) <<< _.endDate) =<< editing

temporaryAddressChangeComponent :: Types.Self -> Maybe User.PendingAddressChange -> JSX
temporaryAddressChangeComponent self@{ props: props@{ subscription: { package } } }  editing =
  TemporaryAddressChange.temporaryAddressChange
    { subsno: props.subscription.subsno
    , cusno: props.user.cusno
    , pastAddresses: readPastTemporaryAddress <$> props.user.pastTemporaryAddresses
    , nextDelivery: toDate =<< toMaybe package.nextDelivery
    , lastDelivery: maximum $ mapMaybe (toDate <=< toMaybe <<< _.nextDelivery) package.products
    , editing: editing
    , userUuid: props.user.uuid
    , now: self.props.now
    , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
    , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
    , onSuccess: \{ pendingAddressChanges: newPendingChanges } ->
                    self.setState _
                        { pendingAddressChanges = toMaybe newPendingChanges
                        , wrapperProgress = AsyncWrapper.Success Helpers.successText
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
  let formatDates { startDate, endDate } = Helpers.formatDateString startDate $ toMaybe endDate
      pauseLine pause =
        let text = "Uppehåll: " <> formatDates pause
        in case Tuple (toDate pause.startDate) (toDate =<< toMaybe pause.endDate) of
          Tuple (Just _startDate) (Just _endDate) ->
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
