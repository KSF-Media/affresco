module MittKonto.Main.UserView.Subscription.Elements where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.JSDate (toDateTime)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DeliveryReclamation as DeliveryReclamation
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
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
import React.Basic.Events (handler_)
import React.Basic.Router as Router

receiverName :: Types.Self -> Array DescriptionList.Definition
receiverName self@{ props: { subscription: { receiver } } } =
  foldMap (\r -> Array.singleton
              { term: "Mottagare:"
              , description: [ DOM.text r ]
              }) $ toMaybe receiver

deliveryAddress :: Types.Self -> Array DescriptionList.Definition
deliveryAddress self@{ props: { subscription: { deliveryAddress: subDeliveryAddress, package }, user: { address: userAddress } } } =
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
paymentMethod self@{ props: props@{ subscription: sub@{ paymentMethod: method } } } = Array.singleton
  { term: "Faktureringsmetoden:"
  , description: [ DOM.text $ Helpers.translatePaymentMethod method ]
  }

pendingAddressChanges :: Types.Self -> Array DescriptionList.Definition
pendingAddressChanges self@{ state: { now, pendingAddressChanges: pendingChanges } } = Array.singleton $
  { term: "Tillfällig adressändring:"
  , description: map (DOM.text <<< Helpers.showPendingAddressChange) (foldMap filterExpiredPendingChanges pendingChanges)
  }
  where
    filterExpiredPendingChanges :: Array User.PendingAddressChange -> Array User.PendingAddressChange
    filterExpiredPendingChanges changes =
      case now of
        Nothing  -> changes
        Just date -> filter (not Helpers.isPeriodExpired date <<< toMaybe <<< _.endDate) changes

billingDateTerm :: Types.Self -> Array DescriptionList.Definition
billingDateTerm self@{ props: { subscription: { dates: { end } } } } = foldMap
  (\e -> Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text $ e ]
      }
  ) $ trim <$> (Helpers.formatDate =<< toMaybe end)

subscriptionEndTerm :: Types.Self -> Array DescriptionList.Definition
subscriptionEndTerm self@{ props: { subscription: { dates: { suspend } } } } = foldMap
  (\s -> Array.singleton $
      { term: "Prenumerationens slutdatum:"
      , description: [ DOM.text s ]
      }
  ) $ trim <$> (Helpers.formatDate =<< toMaybe suspend)

subscriptionUpdates :: Types.Self -> JSX
subscriptionUpdates self@{ props: props@{ subscription: sub@{ package } }, state } =
  Grid.row_ [ actionsWrapper ]
  where
    actionsWrapper = ActionsWrapper.actionsWrapper
      { actions: defaultActions <> extraActions
      , wrapperState: self.state.wrapperProgress
      , onTryAgain: self.setState _ { wrapperProgress = updateProgress }
      , containerClass: "subscription--actions-container flex"
      }

    defaultActions =
      [ pauseIcon
      , if maybe true Array.null self.state.pausedSubscriptions
          then mempty
          else removeSubscriptionPauses
      , temporaryAddressChangeIcon
      , case self.state.pendingAddressChanges of
              Just a -> removeTempAddressChanges a
              Nothing -> mempty
      , deliveryReclamationIcon
      ]

    extraActions =
      [ case sub.paymentMethod of
          CreditCard -> creditCardUpdateIcon
          _          -> mempty
      ]

    updateProgress =
      case state.updateAction of
        Just Types.PauseSubscription      -> AsyncWrapper.Editing pauseSubscriptionComponent
        Just Types.TemporaryAddressChange -> AsyncWrapper.Editing temporaryAddressChangeComponent
        Just Types.DeliveryReclamation    -> AsyncWrapper.Editing deliveryReclamationComponent
        Nothing                           -> AsyncWrapper.Ready

    temporaryAddressChangeComponent =
      TemporaryAddressChange.temporaryAddressChange
        { subsno: props.subscription.subsno
        , cusno: props.user.cusno
        , pastAddresses: readPastTemporaryAddress <$> props.user.pastTemporaryAddresses
        , nextDelivery: toDateTime =<< toMaybe package.nextDelivery
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \{ pendingAddressChanges: newPendingChanges } ->
                        self.setState _
                            { pendingAddressChanges = toMaybe newPendingChanges
                            , wrapperProgress = AsyncWrapper.Success successText
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

    readPastTemporaryAddress tmp =
      { streetAddress: Just tmp.street
      , zipCode: Just tmp.zipcode
      , cityName: toMaybe tmp.cityName
      , countryCode: Just tmp.countryCode
      , temporaryName: toMaybe tmp.temporaryName
      }

    pauseSubscriptionComponent =
      PauseSubscription.pauseSubscription
        { subsno: props.subscription.subsno
        , cusno: props.user.cusno
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \pausedSubscription ->
                        self.setState _
                        { pausedSubscriptions = toMaybe pausedSubscription.paused
                        , wrapperProgress = AsyncWrapper.Success successText
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
                  $ Error.subscriptionError Error.SubscriptionPause
                  $ show err
              -- Other cases are not really errors we want notifications from
              _ -> self.props.logger.log (show err) Sentry.Info
            self.setState _ { wrapperProgress = AsyncWrapper.Error errMsg }
        }

    deliveryReclamationComponent =
      DeliveryReclamation.deliveryReclamation
        { subsno:   props.subscription.subsno
        , cusno:    props.user.cusno
        , userUuid: props.user.uuid
        , onCancel: self.setState _ { wrapperProgress = AsyncWrapper.Ready }
        , onLoading: self.setState _ { wrapperProgress = AsyncWrapper.Loading mempty }
        , onSuccess: \_ ->
                        self.setState _
                            { wrapperProgress = AsyncWrapper.Success successText
                            }
        , onError: \err -> do
            self.props.logger.error $ Error.subscriptionError Error.SubscriptionReclamation $ show err
            self.setState _ { wrapperProgress = AsyncWrapper.Error "Något gick fel. Vänligen försök pånytt, eller ta kontakt med vår kundtjänst." }
        }

    successText = Just "Tack, åtgärden lyckades!"

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
            , wrapperProgress = AsyncWrapper.Editing pauseSubscriptionComponent
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
                Left err -> liftEffect do
                  self.setState _
                    { wrapperProgress = AsyncWrapper.Error "Uppehållet kunde inte tas bort. Vänligen kontakta kundtjänst." }
                  Tracking.unpauseSubscription props.user.cusno (show props.subscription.subsno) "error"
                Right newSubscription -> liftEffect do
                  self.setState _
                    { pausedSubscriptions = toMaybe newSubscription.paused
                    , wrapperProgress = AsyncWrapper.Success $ Just "Uppehållet har tagits bort"
                    }
                  Tracking.unpauseSubscription props.user.cusno (show props.subscription.subsno) "success"
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
                , wrapperProgress = AsyncWrapper.Editing temporaryAddressChangeComponent
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
                  let startDate = toDateTime tempAddressChange.startDate
                  let endDate   = toDateTime =<< toMaybe tempAddressChange.endDate
                  case startDate, endDate of
                    (Just startDate'), endDate' -> do
                      tempAddressChangesDeleted <- User.deleteTemporaryAddressChange props.user.uuid props.subscription.subsno startDate' endDate'
                      case tempAddressChangesDeleted of
                        Right newSubscription -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Success $ Just "Tillfällig adressändring har tagits bort",
                            pendingAddressChanges = toMaybe newSubscription.pendingAddressChanges }
                          Tracking.deleteTempAddressChange (show props.subscription.cusno) (show props.subscription.subsno) startDate' endDate' "success"
                        Left _ -> liftEffect do
                          self.setState _
                            { wrapperProgress = AsyncWrapper.Error "Tillfälliga addressförändringar kunde inte tas bort. Vänligen kontakta kundtjänst." }
                          Tracking.deleteTempAddressChange (show props.subscription.cusno) (show props.subscription.subsno) startDate' endDate' "success"
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
        , children: [ Router.link
                        { to: { pathname: "/kreditkort/uppdatera"
                              , state: {}
                              }
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
                        , className: mempty
                        }
                    ]
        }

-- NOTE: We have a rule in our company policy that states that subscription pauses should be 7 days apart.
-- Thus, if a customer wants to extend a pause, they can't do it by adding a new pause immediately after it.
-- This is why we tell the customer to delete the pause and create a new one.
pauseDescription :: JSX
pauseDescription = DOM.div
                      { className: "mitt-konto--note"
                      , children: [ DOM.text "Om du vill ändra på din tillfälliga adressändring eller ditt uppehåll, vänligen radera den gamla först och lägg sedan in den nya." ]
                      }