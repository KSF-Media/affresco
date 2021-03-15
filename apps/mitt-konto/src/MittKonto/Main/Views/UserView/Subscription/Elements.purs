module MittKonto.Main.UserView.Subscription.Elements where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.JSDate (toDateTime)
import Data.List (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Api.Subscription (SubscriptionPaymentMethod(..), isSubscriptionPausable, isSubscriptionTemporaryAddressChangable)
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
pendingAddressChanges self@{ state: { now, pendingAddressChanges: pendingChanges } } =
  if Array.null filteredChanges then mempty else Array.singleton $
  { term: "Tillfällig adressändring:"
  , description: map (showPendingAddressChange self) filteredChanges
  }
  where
    filteredChanges = foldMap filterExpiredPendingChanges pendingChanges
    filterExpiredPendingChanges :: Array User.PendingAddressChange -> Array User.PendingAddressChange
    filterExpiredPendingChanges changes =
      case now of
        Nothing  -> changes
        Just date -> filter (not Helpers.isPeriodExpired date <<< toMaybe <<< _.endDate) changes

showPendingAddressChange :: Types.Self -> User.PendingAddressChange -> JSX
showPendingAddressChange self change@{ address, startDate, endDate } =
  let addressString = Helpers.formatAddress address
      pendingPeriod = Helpers.formatDateString startDate (toMaybe endDate)
  in DOM.span
       { className: "subscription--edit-pending-address-change"
       , children: [ DOM.text $ addressString <> " (" <> pendingPeriod <> ")" ]
       , onClick: handler_ do
           self.setState _
             { updateAction = Just $ Types.EditTemporaryAddressChange change
             , wrapperProgress = AsyncWrapper.Editing $ temporaryAddressChangeComponent self $ Just change
             }
       }

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
subscriptionUpdates self@{ props: props@{ subscription: sub@{ subsno, package } }, state } =
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
      , if maybe true Array.null self.state.pausedSubscriptions
          then mempty
          else removeSubscriptionPauses
      , if isSubscriptionTemporaryAddressChangable sub then temporaryAddressChangeIcon else mempty
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
                        { to: { pathname: "/prenumerationer/" <> show subsno <> "/kreditkort/uppdatera"
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

pauseSubscriptionComponent :: Types.Self -> Maybe User.PausedSubscription -> JSX
pauseSubscriptionComponent self@{props, state} editing =
  PauseSubscription.pauseSubscription
    { subsno: props.subscription.subsno
    , cusno: props.user.cusno
    , userUuid: props.user.uuid
      -- Make sure that both exist if used
    , oldStart: oldEnd *> oldStart
    , oldEnd: oldStart *> oldEnd
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
    oldStart = (toDateTime <<< _.startDate) =<< editing
    oldEnd = ((toDateTime <=< toMaybe) <<< _.endDate) =<< editing

temporaryAddressChangeComponent :: Types.Self -> Maybe User.PendingAddressChange -> JSX
temporaryAddressChangeComponent self@{ props: props@{ subscription: sub@{ package } } }  editing =
  TemporaryAddressChange.temporaryAddressChange
    { subsno: props.subscription.subsno
    , cusno: props.user.cusno
    , pastAddresses: readPastTemporaryAddress <$> props.user.pastTemporaryAddresses
    , nextDelivery: toDateTime =<< toMaybe package.nextDelivery
    , editing: editing
    , userUuid: props.user.uuid
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

pauseDescription :: JSX
pauseDescription = DOM.div
                      { className: "mitt-konto--note"
                      , children: [ DOM.text "Klicka på uppehållet eller adressförändringen för att göra ändringar." ]
                      }

showPausedDates :: Types.Self -> Array User.PausedSubscription -> Array JSX
showPausedDates self =
  let formatDates { startDate, endDate } = Helpers.formatDateString startDate $ toMaybe endDate
      pauseLine pause =
        let text = "Uppehåll: " <> formatDates pause
        in case Tuple (toDateTime pause.startDate) (toDateTime =<< toMaybe pause.endDate) of
          Tuple (Just startDate) (Just endDate) ->
            DOM.span
              { className: "subscription--edit-subscription-pause"
              , children: [ DOM.text text ]
              , onClick: handler_ do
                  self.setState _
                    { updateAction = Just $ Types.EditSubscriptionPause pause
                    , wrapperProgress = AsyncWrapper.Editing $ pauseSubscriptionComponent self $ Just pause
                    }
              }
          _ -> DOM.text text
  in map pauseLine
