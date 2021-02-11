module MittKonto.Main.UserView.Subscription where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.JSDate (JSDate, toDateTime)
import Data.List (fromFoldable, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Now
import KSF.Api.Subscription (SubscriptionPaymentMethod(..))
import KSF.AsyncWrapper as AsyncWrapper
import KSF.DeliveryReclamation as DeliveryReclamation
import KSF.DescriptionList.Component as DescriptionList
import KSF.Grid as Grid
import KSF.JSError as Error
import KSF.PauseSubscription.Component as PauseSubscription
import KSF.Sentry as Sentry
import KSF.TemporaryAddressChange.Component as TemporaryAddressChange
import KSF.User (User, InvalidDateInput(..))
import KSF.User as User
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import KSF.Tracking as Tracking

type Self = React.Self Props State

type Props =
  { subscription :: User.Subscription
  , user :: User
  , logger :: Sentry.Logger
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , pausedSubscriptions :: Maybe (Array User.PausedSubscription)
  , pendingAddressChanges :: Maybe (Array User.PendingAddressChange)
  , now :: Maybe DateTime
  , updateAction :: Maybe SubscriptionUpdateAction
  }

data SubscriptionUpdateAction
  = PauseSubscription
  | TemporaryAddressChange
  | DeliveryReclamation

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: User.SubscriptionDates
  }

formatDate :: JSDate -> Maybe String
formatDate date = format formatter <$> toDateTime date
  where
    dot = Placeholder "."
    formatter = fromFoldable
      [ DayOfMonthTwoDigits
      , dot
      , MonthTwoDigits
      , dot
      , YearFull
      ]

component :: React.Component Props
component = React.createComponent "Subscription"

subscription :: Props -> JSX
subscription = make component
  { initialState:
      { wrapperProgress: AsyncWrapper.Ready
      , pausedSubscriptions: Nothing
      , pendingAddressChanges: Nothing
      , now: Nothing
      , updateAction: Nothing
      }
  , render
  , didMount
  }

didMount :: Self -> Effect Unit
didMount self = do
  now <- Now.nowDateTime
  self.setState _
    { now = Just now
    , pausedSubscriptions = toMaybe self.props.subscription.paused
    , pendingAddressChanges = toMaybe self.props.subscription.pendingAddressChanges
    }
  self.props.logger.setUser $ Just self.props.user

render :: Self -> JSX
render self@{ props: props@{ subscription: sub@{ package } } } =
  Grid.row2
    (DescriptionList.descriptionList
         { definitions:
             [ { term: "Produkt:"
               , description: [ DOM.text package.name ]
               }
             , { term: "Pren.nr:"
               , description: [ DOM.text $ show sub.subsno ]
               }
             , { term: "Status:"
               , description:
                   [ DOM.text $ translateStatus props.subscription.state ]
                   <> let pausedDates = foldMap (showPausedDates <<< filterExpiredPausePeriods) $ self.state.pausedSubscriptions
                          descriptionText = if Array.null pausedDates
                                            then mempty
                                            else pauseDescription
                       in (map DOM.text pausedDates) <> [ descriptionText ]
               }
             ]
             <> receiverName
             <> deliveryAddress
             <> foldMap pendingAddressChanges self.state.pendingAddressChanges
             <> foldMap billingDateTerm billingPeriodEndDate
             <> foldMap subscriptionEndTerm subscriptionEndDate
             <> paymentMethod
         })
      (if package.digitalOnly
       then mempty
       else subscriptionUpdates)
      $ Just { extraClasses: [ "subscription--container" ] }
  where
    receiverName =
      foldMap (\receiver -> Array.singleton
                            { term: "Mottagare"
                            , description: [ DOM.text receiver ]
                            }) $ toMaybe props.subscription.receiver
    deliveryAddress =
       if package.digitalOnly
       then mempty
       else Array.singleton
              { term: "Leveransadress:"
              , description: [ DOM.text currentDeliveryAddress ]
              }

    paymentMethod = Array.singleton
              { term: "Faktureringsmetoden:"
              , description: [ DOM.text $ translatePaymentMethod props.subscription.paymentMethod ]
              }

    pendingAddressChanges :: Array User.PendingAddressChange -> Array DescriptionList.Definition
    pendingAddressChanges pendingChanges = Array.singleton $
      { term: "Tillfällig adressändring:"
      , description: map (DOM.text <<< showPendingAddressChange) (filterExpiredPendingChanges pendingChanges)
      }

    billingDateTerm :: String -> Array DescriptionList.Definition
    billingDateTerm date = Array.singleton $
      { term: "Faktureringsperioden upphör:"
      , description: [ DOM.text date ]
      }

    subscriptionEndTerm :: String -> Array DescriptionList.Definition
    subscriptionEndTerm date = Array.singleton $
      { term: "Prenumerationens slutdatum:"
      , description: [ DOM.text date ]
      }

    filterExpiredPausePeriods :: Array User.PausedSubscription -> Array User.PausedSubscription
    filterExpiredPausePeriods pausedSubs =
      case self.state.now of
        Nothing  -> pausedSubs
        Just now -> filter (not isPeriodExpired now <<< toMaybe <<< _.endDate) pausedSubs

    filterExpiredPendingChanges :: Array User.PendingAddressChange -> Array User.PendingAddressChange
    filterExpiredPendingChanges pendingChanges =
      case self.state.now of
        Nothing  -> pendingChanges
        Just now -> filter (not isPeriodExpired now <<< toMaybe <<< _.endDate) pendingChanges

    subscriptionUpdates :: JSX
    subscriptionUpdates =
        Grid.row_ [ asyncWrapper ]
        where
          asyncWrapper = AsyncWrapper.asyncWrapper
            { wrapperState: self.state.wrapperProgress
            , readyView: actionsContainer $ defaultActions
            , editingView: identity
            , successView: \msg -> actionsContainer $ defaultActions <> [successWrapper msg]
            , errorView: \err -> actionsContainer $ defaultActions <> [errorWrapper err]
            , loadingView: identity
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

          successWrapper msg =
            DOM.div { className: "subscription--action-item"
                    , children: [ successContainer [ DOM.div { className: "subscription--update-success check-icon" }
                                                   , foldMap successMessage msg
                                                   ]
                                ]
                    }
          errorWrapper err =
            DOM.div { className: "subscription--action-item"
                    , children: [ errorContainer [ errorMessage err, tryAgain ] ]
                    }
          successMessage msg =
            DOM.div
              { className: "success-text"
              , children: [ DOM.text msg ]
              }

          errorMessage msg =
            DOM.div
              { className: "error-text"
              , children: [ DOM.text msg ]
              }
          tryAgain =
            DOM.span
              { className: "subscription--try-update-again"
              , children: [ DOM.text "Försök igen" ]
              , onClick: handler_ $ self.setState _ { wrapperProgress = updateProgress }
              }
            where
              updateProgress =
                case self.state.updateAction of
                  Just PauseSubscription      -> AsyncWrapper.Editing pauseSubscriptionComponent
                  Just TemporaryAddressChange -> AsyncWrapper.Editing temporaryAddressChangeComponent
                  Just DeliveryReclamation    -> AsyncWrapper.Editing deliveryReclamationComponent
                  Nothing                     -> AsyncWrapper.Ready

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

    actionsContainer children =
      DOM.div { className: "subscription--actions-container flex", children }

    successContainer children =
      DOM.div { className: "subscription--success-container flex", children }

    successText = Just "Tack, åtgärden lyckades!"

    errorContainer children =
      DOM.div { className: "subscription--error-container flex", children }

    loadingSpinner = [ DOM.div { className: "tiny-spinner" } ]

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
            { updateAction = Just PauseSubscription
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
              { updateAction = Just TemporaryAddressChange
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
              { updateAction = Just DeliveryReclamation
              , wrapperProgress = AsyncWrapper.Editing deliveryReclamationComponent
              }

    billingPeriodEndDate =
          map trim $ formatDate =<< toMaybe props.subscription.dates.end

    subscriptionEndDate =
          map trim $ formatDate =<< toMaybe props.subscription.dates.suspend

    -- NOTE: We have a rule in our company policy that states that subscription pauses should be 7 days apart.
    -- Thus, if a customer wants to extend a pause, they can't do it by adding a new pause immediately after it.
    -- This is why we tell the customer to delete the pause and create a new one.
    pauseDescription = DOM.div
                         { className: "mitt-konto--note"
                         , children: [ DOM.text "Om du vill ändra på din tillfälliga adressändring eller ditt uppehåll, vänligen radera den gamla först och lägg sedan in den nya." ]
                         }

    currentDeliveryAddress :: String
    currentDeliveryAddress
      | Just address <- toMaybe props.subscription.deliveryAddress
      = formatAddress address
      | Just { streetAddress, zipCode, city } <- toMaybe props.user.address
      = intercalate ", "
          [ streetAddress
          , fromMaybe "-" $ toMaybe zipCode
          , fromMaybe "-" $ toMaybe city
          ]
      | otherwise = "-"

formatAddress :: User.DeliveryAddress -> String
formatAddress { temporaryName, streetAddress, zipcode, city } =
  (maybe "" (_ <> ", ") $ toMaybe temporaryName) <>
  intercalate ", " [ fromMaybe "-" $ toMaybe streetAddress, zipcode, fromMaybe "-" $ toMaybe city ]

-- | Translates English status to Swedish.
-- | Described in https://git.ksfmedia.fi/taco/faro/blob/master/kayak-api-details.md
translateStatus :: User.SubscriptionState -> String
translateStatus (User.SubscriptionState englishStatus) = do
  case englishStatus of
    "Upcoming"                  -> "Under behandling"
    "Active"                    -> "Aktiv"
    "Paused"                    -> "Uppehåll"
    "Ended"                     -> "Avslutad"
    "UnpaidAndCanceled"         -> "Obetald faktura, avslutad prenumeration."
    "Canceled"                  -> "Avbeställd"
    "CanceledWithLatePayment"   -> "Avslutad efter förfallen faktura."
    "RestartedAfterLatePayment" -> "Aktiverad"
    "DeactivatedRecently"       -> "Förnyad tillsvidare"
    "Unknown"                   -> "Okänd"
    _                           -> englishStatus

translatePaymentMethod :: SubscriptionPaymentMethod -> String
translatePaymentMethod paymentMethod =
  case paymentMethod of
    PaperInvoice         -> "Pappersfaktura"
    CreditCard           -> "Kreditkort"
    NetBank              -> "Netbank"
    ElectronicInvoice    -> "Nätfaktura"
    DirectPayment        -> "Direktbetalning"
    UnknownPaymentMethod -> "Okänd"

isPeriodExpired :: DateTime -> Maybe JSDate -> Boolean
isPeriodExpired baseDate endDate =
  case endDate of
    -- If there's no end date, the period is ongoing
    Nothing   -> false
    Just date ->
      let endDateTime = toDateTime date
      in maybe true (_ < baseDate) endDateTime

showPausedDates :: Array User.PausedSubscription -> Array String
showPausedDates pausedSubs =
  let formatDates { startDate, endDate } = formatDateString startDate $ toMaybe endDate
  in map (((<>) "Uppehåll: ") <<< formatDates) pausedSubs

showPendingAddressChange :: User.PendingAddressChange -> String
showPendingAddressChange { address, startDate, endDate } =
  let addressString = formatAddress address
      pendingPeriod = formatDateString startDate (toMaybe endDate)
  in addressString <> " (" <> pendingPeriod <> ")"

formatDateString :: JSDate -> Maybe JSDate -> String
formatDateString startDate endDate
  | Just startString <- formatDate startDate =
    let endString = fromMaybe "" $ formatDate =<< endDate
    in startString <> " – " <> endString
  | otherwise = mempty
