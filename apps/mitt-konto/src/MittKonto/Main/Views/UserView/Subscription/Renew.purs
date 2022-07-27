module MittKonto.Main.UserView.Subscription.Renew where

import Prelude

import Bottega (BottegaError(..))
import Bottega.Models (FailReason(..), OrderState(..), PaymentMethod(..), PaymentTerminalUrl)
import Bottega.Poller as Poller
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust, isNothing)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Api.Subscription as Subscription
import KSF.Api.Subscription (toString) as Subsno
import KSF.Grid as Grid
import KSF.Helpers as Helpers
import KSF.InputField (inputLabel)
import KSF.InputField.Checkbox as Checkbox
import KSF.User as User
import MittKonto.Main.UserView.Subscription.Types as Types
import MittKonto.Main.UserView.Subscription.Types (RenewSubscription)
import MittKonto.Wrappers.Elements as WrapperElements
import React.Basic (JSX)
import React.Basic.Hooks (Component, useEffect, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)

-- TODO This module has a lot of overlap with Prenumerera.Page.Payment.

type Form =
  { months :: Int
  , cents :: Int
  , shownPrice :: Int
  , paymentMethod :: PaymentMethod
  , confirm :: Maybe Boolean
  }

type State =
  { form :: AsyncWrapper.Progress Form
  , netsUrl :: Maybe PaymentTerminalUrl
  , orderState :: Either BottegaError OrderState
  , poller :: Poller.Poller
  }

paperInvoiceFeeCents :: Int
paperInvoiceFeeCents = 500

component :: Component Types.RenewSubscription
component = do
  poller <- Poller.new
  React.component "RenewSubscription" \props -> React.do
    let firstOffer = Array.head props.subscription.package.offers
        initialPaymentMethod =
          case props.subscription.paymentMethod of
            Subscription.CreditCard -> CreditCard
            Subscription.UnknownPaymentMethod -> CreditCard
            _ -> if isJust $ toMaybe $ props.user.address then PaperInvoice else CreditCard
        initialCents = maybe 0 (_.totalPrice) firstOffer
        initialForm =
          AsyncWrapper.Editing
            { months: maybe 1 (_.months) firstOffer
            , cents: initialCents
            , shownPrice: initialCents + if initialPaymentMethod == PaperInvoice then paperInvoiceFeeCents else 0
            , paymentMethod: initialPaymentMethod
            , confirm: Nothing
            }
        initialState =
          { form: initialForm
          , netsUrl: Nothing
          , poller
          , orderState: Right OrderUnknownState
          }
    state /\ setState <- useState initialState
    useEffect state.orderState do
      case state.orderState of
        Right OrderCompleted -> do
          setState _ { form = AsyncWrapper.Ready }
          props.onSuccess
        Right OrderCanceled -> do
          props.onCancel
        Left _ -> do
          setState _ { form = AsyncWrapper.Error "" }
          props.onError User.SomethingWentWrong
        _ -> pure unit
      pure (pure unit)
    pure $ render props state setState $ setState _ { form = initialForm }

render :: RenewSubscription -> State -> ((State -> State) -> Effect Unit) -> Effect Unit -> JSX
render props state setState resetForm =
  DOM.div
    { className: "renew-subscription--container"
    , children:
        [ AsyncWrapper.asyncWrapper
            { wrapperState: state.form
            , readyView: renderSuccess
            , editingView: renderForm props state setState
            , loadingView: const renderLoading
            , successView: const renderSuccess
            , errorView: renderError
            }
        ]
    }
  where
    -- TODO
    renderSuccess = WrapperElements.successMessage "success"
    renderError _ = WrapperElements.errorWrapper resetForm $ case state.orderState of
      Right (OrderFailed NetsInternalError) -> "nets internal error"
      Right (OrderFailed NetsIssuerError) -> "nets issuer error"
      Right (OrderFailed NetsCanceled) -> "nets canceled"
      Right (OrderFailed SubscriptionExistsError) -> "subscriptionexistserror"
      Right (OrderFailed SubscriptionError) -> "subscriptionerror"
      Right (OrderFailed OrderNotFound) -> "ordernotfound"
      Right (OrderFailed UnknownReason) -> "Något gick fel."
      Right OrderCanceled -> "Canceled."
      Right OrderUnknownState -> "Något gick fel."
      Left BottegaInsufficientAccount -> "insufficient account"
      Left (BottegaUnexpectedError _) -> "Något gick fel."
      _ -> "Något gick fel."
    renderLoading =
      React.fragment $
        [ DOM.div { className: "tiny-spinner" } ] <>
        (case state.netsUrl of
            Just { paymentTerminalUrl } -> case state.orderState of
              Right OrderCreated ->
                [ DOM.div
                    { className: "payment-wrapper"
                    , children:
                        [ DOM.iframe
                            { src: paymentTerminalUrl
                            , className: "payment-terminal"
                            }
                        ]
                    }
                ]
              Right OrderStarted ->
                [ DOM.div
                    { className: "payment-wrapper"
                    , children:
                        [ DOM.div
                            { className: "payment-terminal"
                            , children: [ DOM.text "Vänligen vänta. Vi behandlar din beställning." ]
                            }
                        ]
                    }
                ]
              _ -> mempty
            _ -> mempty
        )

renderForm :: RenewSubscription -> State -> ((State -> State) -> Effect Unit) -> Form -> JSX
renderForm props state setState form =
  DOM.div
    { className: "renew-subscription--container"
    , children:
        [ Grid.row_
            [ DOM.div
                { className: "renew-subscription--header"
                , children: [ DOM.h3_ [ DOM.text "Förnya prenumerationen" ] ]
                }
            , DOM.div
                { className: "renew-subscription--close-icon"
                , children: [ DOM.div { className: "close-icon" } ]
                , onClick: handler_ props.onCancel
                }
            ]
        , DOM.form
            { onSubmit: handler preventDefault $ const submit
            , children:
                [ selectPayment
                , selectOffer
                , if form.paymentMethod == PaperInvoice then paperInvoiceMessage else mempty
                , showDetails
                , if isJust form.confirm then confirmBuy else mempty
                , buyButton
                ]
            }
        ]
    }
  where
    subsnoStr = Subsno.toString props.subscription.subsno
    setFormState = \f -> setState $ \s -> s { form = map f s.form }
    submit = if form.confirm == Just true || form.paymentMethod == CreditCard
             then submitForm props form state setState
             else setFormState _ { confirm = Just false }

    selectPayment =
      DOM.div
        { className: "input-field--container"
        , style: DOM.css { "width": "100%" }
        , children:
            [ inputLabel { label: "Betalningssätt", nameFor: "sub-po-" <> subsnoStr }
            , DOM.select
                { id: "sub-po-" <> subsnoStr
                , children:
                    [ DOM.option
                        { value: "c"
                        , selected: form.paymentMethod == CreditCard
                        , children: [ DOM.text "Kreditkort" ]
                        }
                    ] <>
                    ( if isJust $ toMaybe $ props.user.address
                        then [ DOM.option
                                 { value: "i"
                                 , selected: form.paymentMethod == PaperInvoice
                                 , children: [ DOM.text "Pappersfaktura" ]
                                 }
                             ]
                        else mempty
                    )
                , onChange: handler targetValue
                    (\newMethod -> do
                        let method = decodeMethod newMethod
                        setFormState _ { paymentMethod = method
                                       , confirm = Nothing
                                       , shownPrice = form.cents + if method == PaperInvoice then paperInvoiceFeeCents else 0
                                       }
                    )
                }
            ]
        }

    decodeMethod method = if method == Just "c" then CreditCard else PaperInvoice

    selectOffer =
      DOM.div
        { className: "input-field--container"
        , style: DOM.css { "width": "100%" }
        , children:
            [ inputLabel { label: "Faktureringsperiod", nameFor: "sub-of-" <> subsnoStr }
            , DOM.select
                { id: "sub-of-" <> subsnoStr
                , children: map paymentOption props.subscription.package.offers
                , value: show form.months
                , onChange: handler targetValue $
                    \newMonths -> foldMap setMonthsAndPrice $
                                   Array.find (\m -> Just (show m.months) == newMonths) props.subscription.package.offers
                }
            ]
        }

    setMonthsAndPrice offer =
      setFormState _ { months = offer.months
                     , cents = offer.totalPrice
                     , shownPrice = offer.totalPrice + if form.paymentMethod == PaperInvoice then paperInvoiceFeeCents else 0
                     }

    paperInvoiceMessage =
      DOM.div
        { className: "alert alert--info"
        , children:
            [ DOM.h3_ [ DOM.text "Obs!" ]
            , DOM.p_ [ DOM.text " På pappersfakturor som levereras per post uppbär vi en tilläggsavgift på 5,00 euro per faktura (inkl. Moms). Du kan i din nätbank byta fakturan till en e-faktura som inte har ett faktureringstillägg." ]
            ]
        }

    paymentOption offer =
      DOM.option
        { value: show $ offer.months
        , children: [ DOM.text $ show offer.months <> (if offer.months == 1 then " månad" else " månader") ]
        }

    showDetails =
      DOM.div
        { className: "subscription--renew-price"
        , children:
            [ DOM.h3_ [ DOM.text "Din prenumeration" ]
            , row $ props.subscription.package.name
            , row $ "Pris: " <>
                (maybe "-" (\offer -> Helpers.formatEur offer.totalPrice) activeOffer) <> " €"
            , if form.paymentMethod == PaperInvoice
                then row " + 5€ faktureringstillägg per pappersfaktura"
                else mempty
            , row "Prenumerationstyp: Fortlöpande"
            , DOM.br {}
            , let fn = toMaybe props.user.firstName
                  ln = toMaybe props.user.lastName
              in if isJust (fn <|> ln)
                   then row $ fromMaybe "" fn <> " " <> fromMaybe "" ln
                   else mempty
            , row props.user.email
            ] <> (if props.subscription.package.digitalOnly then mempty
                  else case toMaybe props.user.address of
                    Just address ->
                      [ DOM.br {}
                      , row address.streetAddress
                      , row $ fromMaybe "" $ toMaybe address.zipCode
                      , row $ fromMaybe "" $ toMaybe address.city
                      ]
                    Nothing -> mempty -- Impossible
                 ) <>
            [ DOM.hr { className: "mitt-konto--break" }
            , row $ ("Totalpris: " <> Helpers.formatEur form.shownPrice) <> " €"
            ]
        }
      where
        row t =
          DOM.div
            { className: "subscription--renew-price-row"
            , children: [ DOM.text t ]
            }

    buyButton =
      Grid.columnThird $
        DOM.button
          { type: "submit"
          , children: [ DOM.text "Köp nu" ]
          , className: "button-green"
          , disabled: isNothing activeOffer || form.confirm == Just false
          }

    confirmBuy =
      DOM.div
        { className: "subscription--renewal-confirm"
        , children:
            [ Checkbox.inputCheckbox
                { type_: Checkbox.Checkbox
                , name: "confirm-paper-invoice"
                , id: "confirm-paper-invoice-" <> subsnoStr
                , label: Just "Vänligen bekräfta"
                , checked: fromMaybe false form.confirm
                , onChange: \checked -> setFormState _ { confirm = Just checked }
                }
            ]
        }

    activeOffer =
      Array.find (\o -> form.months == o.months) props.subscription.package.offers

submitForm :: Types.RenewSubscription -> Form -> State -> ((State -> State) -> Effect Unit) -> Effect Unit
submitForm props form state setState = do
  Aff.launchAff_ $ do
    eitherNetsUrl <- runExceptT do
      let newOrder =
            { packageId: props.subscription.package.id
            , period: form.months
            , payAmountCents: form.cents
            , campaignNo: Nothing
            , orderSource: Nothing
            }
      order <- ExceptT (User.createOrder newOrder)
      nets <- ExceptT (User.payOrder order.number form.paymentMethod)
      pure $ { nets, order }
    case eitherNetsUrl of
      Left BottegaInsufficientAccount -> do
        liftEffect $ setState _ { form = AsyncWrapper.Error "insufficient account" }
      Left (BottegaUnexpectedError err) -> liftEffect do
        Console.log err
        setState _ { form = AsyncWrapper.Error "Något gick fel." }
      Left BottegaTimeout ->
        liftEffect $ setState _ { form = AsyncWrapper.Error "Timeout hände." }
      Right { nets, order } -> do
        Poller.startOrder state.poller setOrderState order.number
        liftEffect do
          setState _ { form = AsyncWrapper.Loading form
                     , netsUrl = nets
                     }
  where
    setOrderState orderState = setState $ \s -> s { orderState = orderState }
