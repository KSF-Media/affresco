module KSF.Search where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (mapMaybe, drop, take)
import Data.Array as Array
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Foldable (intercalate, length, foldMap, sum, sequence_)
import Data.Int as Int
import Data.JSDate (toDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String.Common as String
import Data.Tuple (Tuple(..), fst, snd)
import Data.UUID (UUID, parseUUID)
import Data.Validation.Semigroup (invalid, isValid, validation)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import KSF.Api.Search (FaroUser, JanrainUser, SearchResult)
import KSF.Api.Subscription (Subscription, isSubscriptionExpired)
import KSF.Api.Subscription (toString) as Subsno
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Grid as Grid
import KSF.Helpers (formatDateDots)
import KSF.InputField as InputField
import KSF.InputField.Checkbox as InputCheckbox
import KSF.Random (randomString)
import KSF.User as User
import KSF.User.Cusno (Cusno(..))
import KSF.User.Cusno as Cusno
import KSF.ValidatableForm (class ValidatableField, ValidatedForm, ValidationError(..), inputFieldErrorMessage, validateEmailAddress, validateField, validatePassword)
import React.Basic (JSX)
import React.Basic.Hooks (Component, component, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, preventDefault)
import React.Basic.Events as Events

type Props =
  { setActiveUser :: UUID -> Effect Unit
  , resetRedirect :: Effect Unit
  , now           :: Date
  }

type SearchActions =
  { setActiveUser      :: UUID -> Effect Unit
  , loadSubs           :: Cusno -> Effect Unit
  , startCreateAccount :: forall a. FaroUser a -> Effect Unit
  , createAccountForm  :: Maybe (Tuple Cusno JSX)
  , startSetCusno      :: JanrainUser -> Effect Unit
  , startPasswordCtrl  :: JanrainUser -> String -> Effect Unit
  , personaUserForm    :: Maybe (Tuple UUID JSX)
  , isEditingAccount   :: Boolean
  }

type TaggedSubscription =
  { sub     :: Subscription
  , expired :: Boolean
  }

data PersonaUserEdit
  = SetCusno (AsyncWrapper.Progress (Maybe Cusno))
  | ControlPassword (AsyncWrapper.Progress String)

search :: Component Props
search = do
  component "Search" \ { setActiveUser, resetRedirect, now } -> React.do
    query /\ setQuery <- useState' Nothing
    results /\ setTaggedResults <- useState Nothing
    (searchWrapper :: AsyncWrapper.Progress JSX) /\ setSearchWrapper <- useState' AsyncWrapper.Ready
    (accountData :: Maybe (Tuple Cusno (AsyncWrapper.Progress User.NewCusnoUser))) /\ setAccountData <-
      useState Nothing
    (personaUserEdit :: Maybe (Tuple UUID PersonaUserEdit)) /\ setPersonaUserEdit <- useState Nothing
    let submitSearch = case query /\ (parseUUID =<< query) of
          Nothing /\ _ -> pure unit
          _ /\ Just uuid -> setActiveUser uuid
          Just q /\ _ -> do
            setSearchWrapper $ AsyncWrapper.Loading mempty
            resetRedirect
            Aff.launchAff_ do
              queryResult <- User.searchUsers { query: q, faroLimit: 10 }
              case queryResult of
                Right r -> liftEffect do
                  setResults $ const $ Just r
                  setSearchWrapper $ AsyncWrapper.Success Nothing
                Left e -> liftEffect do
                  setResults $ const Nothing
                  setSearchWrapper $ AsyncWrapper.Error e
        resetPersonaUserEdit = setPersonaUserEdit $ const Nothing
        loadSubs cusno = do
          setSearchWrapper $ AsyncWrapper.Loading mempty
          Aff.launchAff_ do
            queryResult <- User.searchUsers { query: Cusno.toString cusno, faroLimit: 1 }
            case queryResult of
              Right res -> liftEffect $ sequence_ $
                             map (\r -> setResults $ map (map (updateSubs cusno r))) $
                             Array.concatMap (_.faro) res
              Left _ -> pure unit
            liftEffect $ setSearchWrapper $ AsyncWrapper.Success Nothing
        startCreateAccount usr@{ cusno, email } = do
          pw <- randomString 10
          nowISO <- JSDate.toISOString =<< JSDate.now
          let legalConsent =
                { consentId: "legal_acceptance_v1"
                , screenName: "legalAcceptanceScreen"
                , dateAccepted: nowISO
                }
          setAccountData $ const $ Just $ Tuple cusno $ AsyncWrapper.Editing
            { cusno: cusno
            , email: fromMaybe "" email
            , firstName: ""
            , lastName: ""
            , password: pw
            , consents: [legalConsent]
            , sendReset: true
            }
        submitCreateAccount newUser = Aff.launchAff_ do
          result <- User.createCusnoUser newUser
          let setError err = setAccountData $ (map <<< map) (const $ AsyncWrapper.Error err)
          liftEffect $ case result of
            Left User.RegistrationEmailInUse ->
              setError "E-postadressen är redan i bruk."
            Left (User.RegistrationCusnoInUse conflicting) -> do
              setError $ "Kundnummer är redan i bruk."
                <> foldMap (\x -> " (" <> x <> ")") conflicting.email
            Left _ ->
              setError "Något gick fel."
            Right res -> do
              let faroCusno x = map _.cusno $ Array.head x.faro
                  updateWithJanrain :: SearchResult Subscription -> SearchResult Subscription -> SearchResult Subscription
                  updateWithJanrain x state =
                    if isNothing state.janrain && faroCusno state == faroCusno x
                      then x
                      else state
              setResults $ (map <<< map) $ updateWithJanrain res
              setAccountData $ const Nothing
        createAccountForm = (map <<< map)
                            (renderEditNewUser submitCreateAccount (setAccountData (const Nothing)) $
                             setAccountData <<< map <<< map <<< map) accountData
        isEditingAccount = case accountData of
          Just (Tuple _ (AsyncWrapper.Editing _)) -> true
          _ -> false
        startSetCusno :: JanrainUser -> Effect Unit
        startSetCusno user = do
          setPersonaUserEdit $ const $ Just $ Tuple user.uuid $ SetCusno $
            AsyncWrapper.Editing $ Cusno.fromString =<< user.cusno
        submitSetCusno uuid cusno = do
          setPersonaUserEdit <<< map <<< map $ const $
            SetCusno $ AsyncWrapper.Loading $ Just cusno
          Aff.launchAff_ do
            result <- User.setCusno uuid cusno
            let setError err = setPersonaUserEdit $ (map <<< map <<< mapSetCusno) $
                               const $ AsyncWrapper.Error err
                success = do
                  setSearchWrapper $ AsyncWrapper.Success Nothing
                  setPersonaUserEdit $ (map <<< map <<< mapSetCusno) $
                    const $ AsyncWrapper.Success Nothing
            case result of
              Left (User.RegistrationCusnoInUse conflicting) -> liftEffect do
                setError $ "Kundnummer är redan i bruk."
                  <> foldMap (\x -> " (" <> x <> ")") conflicting.email
              Left User.InvalidCusno -> liftEffect do
                setError $ "Ingen Kayak konto med detta kundnummer"
              Left _ -> liftEffect do
                setError "Något gick fel."
              Right _ -> do
                liftEffect $ setSearchWrapper $ AsyncWrapper.Loading mempty
                queryResult <- User.searchUsers { query: Cusno.toString cusno, faroLimit: 1 }
                liftEffect $ case Array.take 1 <$> queryResult of
                  Right [r] -> do
                    setResults $ map $ map
                      -- Replace this uuid with new load
                      (\x -> if (_.uuid <$> x.janrain) == Just uuid then r else x) >>>
                      -- Filter this cusno from Faro only results
                      Array.filter (\x -> isJust x.janrain || Just cusno /= (_.cusno <$> Array.head x.faro))
                    success
                  Right _ -> success
                  Left err -> do
                    setPersonaUserEdit $ (map <<< map <<< mapSetCusno) $ const $ AsyncWrapper.Error $ "Något gick fel. " <> show err
        startPasswordCtrl :: JanrainUser -> String -> Effect Unit
        startPasswordCtrl user email = do
          setPersonaUserEdit $ const $ Just $ Tuple user.uuid $ ControlPassword $
            (AsyncWrapper.Editing email)
        resetPassword :: String -> Effect Unit
        resetPassword email = do
          setPersonaUserEdit <<< map <<< map $ const $
            ControlPassword $ AsyncWrapper.Loading email
          Aff.launchAff_ do
            result <- User.requestPasswordReset email
            liftEffect $ case result of
              Left _ -> setPersonaUserEdit <<< map <<< map <<< mapPasswordControl $
                        const $ AsyncWrapper.Error ""
              Right _ -> setPersonaUserEdit $ (map <<< map <<< mapPasswordControl) $
                         const $ AsyncWrapper.Success Nothing
        -- At most one Persona user has a form attached to it.
        personaUserForm :: Maybe (Tuple UUID JSX)
        personaUserForm = map renderUserForm personaUserEdit
        renderUserForm :: Tuple UUID PersonaUserEdit -> Tuple UUID JSX
        renderUserForm (Tuple uuid (SetCusno wrp)) =
          Tuple uuid $ renderSetCusno (submitSetCusno uuid) resetPersonaUserEdit
          (setPersonaUserEdit <<< map <<< map <<< mapSetCusno <<< map) wrp
        renderUserForm (Tuple uuid (ControlPassword wrp)) =
          Tuple uuid $ renderControlPassword resetPassword resetPersonaUserEdit wrp
        tagExpired :: Subscription -> TaggedSubscription
        tagExpired sub =
          { sub
          , expired: isSubscriptionExpired sub now
          }
        tag :: FaroUser Subscription -> FaroUser TaggedSubscription
        tag u = u { subs = (map <<< map) tagExpired u.subs }
        unTag :: FaroUser TaggedSubscription -> FaroUser Subscription
        unTag u = u { subs = (map <<< map) _.sub u.subs }
        setResults :: (Maybe (Array (SearchResult Subscription)) -> Maybe (Array (SearchResult Subscription))) -> Effect Unit
        setResults f = setTaggedResults $
                       -- Remove tag
                       (map <<< map) (\r -> r { faro = map unTag r.faro } ) >>>
                       -- Apply transformation
                       f >>>
                       -- Tag result
                       (map <<< map) (\r -> r { faro = map tag r.faro })
        actions =
          { setActiveUser
          , loadSubs
          , startCreateAccount
          , createAccountForm
          , startSetCusno
          , startPasswordCtrl
          , personaUserForm
          , isEditingAccount
          }
    pure $ React.fragment
      [ DOM.div { className: "search--container"
                , children: [ searchQuery query setQuery submitSearch
                            , searchResults actions searchWrapper results
                            ]
                }
      ]
  where
    mapSetCusno f (SetCusno c) = SetCusno $ f c
    mapSetCusno _ x = x

    mapPasswordControl f (ControlPassword c) = ControlPassword $ f c
    mapPasswordControl _ x = x

    updateSubs :: forall a. Cusno -> FaroUser a -> SearchResult a -> SearchResult a
    updateSubs cusno result state =
      state { faro = map (\x -> if x.cusno == cusno then result else x) state.faro }

    searchQuery query setQuery submitSearch =
      DOM.div
        { className: "search--query mitt-konto--container clearfix"
        , children:
            [ DOM.span
                { className: "mitt-konto--component-heading"
                , children: [ DOM.text "Sök"]
                }
            , DOM.form
                { className: "search--query-form mitt-konto--component-block-content"
                , children:
                    [ DOM.span
                        { className: "search--query-input"
                        , children:
                            [ InputField.inputField
                                { type_: InputField.Text
                                , name: "query"
                                , placeholder: "Sök"
                                , label: Nothing
                                , value: query
                                , onChange: setQuery
                                , validationError: Nothing
                                }
                            ]
                        }
                    , DOM.button
                        { type: "submit"
                        , children: [ DOM.text "Sök" ]
                        , className: "button-green search--query-submit"
                        , disabled: isNothing query || query == Just ""
                        }
                    ]
                , onSubmit: Events.handler preventDefault
                    $ \_ -> submitSearch
                }
            ]
        }

    searchResults actions searchWrapper results =
      AsyncWrapper.asyncWrapper
        { wrapperState: searchWrapper
        , readyView: searchInitial
        , editingView: \_ -> searchDone actions results
        , loadingView: searchLoading
        , successView: \_ -> searchDone actions results
        , errorView: searchError
        }

    searchInitial = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: [ DOM.text "Mata in e-post, namn (efternamn förnamn), cusno eller subsno (p:nnn)" ]
      }

    searchDone actions results = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children: case results of
          Nothing -> [ DOM.text "ingen" ]
          Just x -> [ DOM.div
                        { className: "search--results-container"
                        , children:
                            [ DOM.table
                                { className: "search--results-table"
                                , children: [  DOM.thead_ [ headerRow1 ]
                                            ,  DOM.tbody_ $ join $ renderResult actions <$> x
                                            ]
                                }
                            ]
                        }
                    ]
      }

    searchLoading spinner = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children:
          [ DOM.div
              { className: "search--spinner-container"
              , children: [ spinner ]
              }
          , DOM.div_ [ DOM.text "Söker" ]
          ]
      }

    searchError msg = DOM.div
      { className: "search--search-results mitt-konto--component-block-content"
      , children:
          [ DOM.div_ [ DOM.text "Något gick fel" ]
          , DOM.div_ [ DOM.text msg ]
          ]
      }

    headerRow1 =
      DOM.tr
        { children:
            [ DOM.th_ [ DOM.text "Aktion" ]
            , DOM.th_ [ DOM.text "Cusno" ]
            , DOM.th_ [ DOM.text "E-post" ]
            , DOM.th { colSpan: 2, children: [ DOM.text "Namn" ] }
            , DOM.th_ [ DOM.text "Adress" ]
            , DOM.th { colSpan: 3
                     , children: [ DOM.text "Prenumerationer" ]
                     }
            ]
        }

    renderResult actions { janrain, faro } =
      foldMap (renderJanrain
                 actions.setActiveUser
                 actions.startSetCusno
                 actions.startPasswordCtrl
                 faro) janrain <>
      (Array.concatMap (\usr@{ cusno } ->
                         renderFaro
                           (actions.loadSubs cusno)
                           (actions.startCreateAccount usr)
                           (isNothing janrain)
                           (actions.isEditingAccount &&
                            (fst <$> actions.createAccountForm) == Just cusno)
                           usr) faro) <>
      (if isNothing janrain && (fst <$> actions.createAccountForm) == (_.cusno <$> Array.head faro)
         then pure $ foldMap (interruptForm <<< snd) actions.createAccountForm
         else mempty) <>
      (if (_.uuid <$> janrain) == (fst <$> actions.personaUserForm)
         then pure $ foldMap (interruptForm <<< snd) actions.personaUserForm
         else mempty)

    interruptForm form = DOM.tr_ [ DOM.td { colSpan: 9, children: [ form ] } ]

    renderJanrain :: forall a. (UUID -> Effect Unit) -> (JanrainUser -> Effect Unit) -> (JanrainUser -> String -> Effect Unit) -> Array (FaroUser a) -> JanrainUser -> Array JSX
    renderJanrain setActiveUser startSetCusno startPasswordCtrl faroResults user = pure $
      DOM.tr
        { className: "search--item-identity"
        , children:
            [ DOM.td
                { rowSpan: 1+(sum $ map faroLength faroResults)
                , children:
                    [ DOM.button
                        { onClick: Events.handler_ $ setActiveUser user.uuid
                        , children: [ DOM.text "Visa konto som kund" ]
                        }
                    , DOM.button
                        { onClick: Events.handler_ $ startSetCusno user
                        , children: [ DOM.text "Redigera kundnummer" ]
                        }
                    , foldMap (\email ->
                                DOM.button
                                  { onClick: Events.handler_ $ startPasswordCtrl user email
                                  , children: [ DOM.text "Kontrollera lösenord" ]
                                  }) user.email
                    ]
                }
            , DOM.td_
                [ intercalate (DOM.text " / ") $
                    (foldMap (Array.singleton <<< DOM.text) user.cusno) <>
                    (foldMap (map (DOM.i_ <<< pure <<< DOM.text)) user.otherCusnos)
                ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.email ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.firstName ]
            , DOM.td_ [ DOM.text $ fromMaybe "vet ej" user.lastName ]
            , DOM.td
                { colSpan: 4
                , children: [ ]
                }
            ]
        }
      where
        faroLength x = maybe 1 (\s -> if Array.null s then 1 else length s) x.subs

    renderFaro :: Effect Unit -> Effect Unit -> Boolean -> Boolean -> FaroUser TaggedSubscription -> Array JSX
    renderFaro loadSubs startCreateAccount standalone isEditingThis user =
      [ DOM.tr
          { className: if standalone then "search--standalone-cusno" else "search--sub-cusno"
          , children:
              (if standalone
                 then [ td [ DOM.button
                               { onClick: Events.handler_ startCreateAccount
                               , children: [ DOM.text "Skapa konto" ]
                               , disabled: isEditingThis
                               }
                           ]
                      ]
                 else mempty) <>
              [ td [ DOM.text $ Cusno.toString user.cusno ]
              , td [ DOM.text $ fromMaybe "vet ej" user.email ]
              , DOM.td
                  { rowSpan: rowSpan
                  , colSpan: 2
                  , children: [ DOM.text user.name ]
                  }
              , td $ maybe
                  [ DOM.text "vet ej" ]
                  (\address -> intercalate [DOM.br {}] $
                                 [ [ DOM.text address.streetAddress ]
                                 , [ DOM.text $ String.joinWith " " $
                                       mapMaybe toMaybe [ address.zipCode, address.city ] ]
                                 ] ) $
                  user.address
              ] <>
              case user.subs of
                Nothing -> [ loadableSubs user.cusno ]
                Just subs -> if Array.null subs
                               then [ DOM.td { colSpan: 3
                                             , children: [ DOM.text "Ingen" ]
                                             }
                                    ]
                               else subscriptionRow =<< take 1 subs
          }
      ] <> maybe mempty (\s -> subtr <<< subscriptionRow <$> drop 1 s) user.subs
      where
        td children = DOM.td { rowSpan: rowSpan, children: children }
        rowSpan = maybe 1 (\s -> if Array.null s then 1 else length s) user.subs
        subtr children =  DOM.tr { className: "search--subrow", children: children }
        subscriptionRow { sub, expired } =
          [ DOM.td
              { className: "search--result-subsno-column" <>
                           if expired then " search--result-expired" else ""
              , children: [ DOM.text $ Subsno.toString sub.subsno ]
              }
          , DOM.td
              { className: if expired then "search--result-expired" else ""
              , children: [ DOM.text $ sub.package.name ]
              }
          , DOM.td
              { className: if expired then "search--result-expired" else ""
              , children: [ DOM.text $ case Tuple (toDate sub.dates.start) leastEnd of
                               Tuple Nothing Nothing -> "ogiltig"
                               Tuple start end -> maybe "" formatDateDots start <> "–" <>
                                                  maybe "" formatDateDots end
                          ]
              }
          ]
          where
            leastEnd = case Tuple (toDate =<< toMaybe sub.dates.suspend) (toDate =<< toMaybe sub.dates.end) of
              Tuple (Just end1) (Just end2) -> Just $ min end1 end2
              Tuple end1 end2 -> end1 <|> end2
        loadableSubs cusno =
           DOM.td { colSpan: 3
                  , children:
                      [ DOM.i
                          { className: "selectable"
                          , children: [ DOM.text "Klicka för att ladda" ]
                          , onClick: Events.handler_ loadSubs
                          }
                      ]
                  }

data NewUserFields
  = EmailField
  | PasswordField
derive instance eqNewUserFields :: Eq NewUserFields

instance validatableFieldNewUserFormFields :: ValidatableField NewUserFields where
  validateField field value _serverErrors = case field of
    EmailField -> validateEmailAddress field value
    PasswordField -> validatePassword field value

renderEditNewUser
  :: (User.NewCusnoUser -> Effect Unit)
  -> Effect Unit
  -> ((User.NewCusnoUser -> User.NewCusnoUser) -> Effect Unit)
  -> AsyncWrapper.Progress User.NewCusnoUser
  -> JSX
renderEditNewUser submitNewAccount cancel setAccountData wrapperState =
  AsyncWrapper.asyncWrapper
    { wrapperState
    , readyView: mempty
    , editingView: render
    , loadingView: const $ DOM.div { className: "tiny-spinner" }
    , successView: const mempty
    , errorView: editError
    }
  where
    render account =
      DOM.form
        { className: "search--create-account"
        , onSubmit: Events.handler preventDefault $ const $ submit validatedForm
        , children:
            [ Grid.row_
                [ DOM.text $ "Kundnummer: " <> Cusno.toString account.cusno ]
            , Grid.row_
                [ InputField.inputField
                    { type_: InputField.Email
                    , name: "email"
                    , placeholder: "E-post"
                    , value: Just account.email
                    , onChange: \newEmail -> setAccountData _ { email = fromMaybe "" newEmail }
                    , label: Just "E-post"
                    , validationError: inputFieldErrorMessage $ validateField EmailField (Just account.email) []
                    }
                ]
            , Grid.row2
              (InputField.inputField
                 { type_: InputField.Text
                 , name: "firstName"
                 , placeholder: "Förnamn"
                 , value: Just account.firstName
                 , onChange: \newName -> setAccountData _ { firstName = fromMaybe "" newName }
                 , label: Just "Förnamn"
                 , validationError: Nothing
                 })
              (InputField.inputField
                 { type_: InputField.Text
                 , name: "lastName"
                 , placeholder: "Efternamn"
                 , value: Just account.lastName
                 , onChange: \newName -> setAccountData _ { lastName = fromMaybe "" newName }
                 , label: Just "Efternamn"
                 , validationError: Nothing
                 }) {}
            , Grid.row_
                [ InputField.inputField
                    { type_: InputField.Text
                    , name: "password"
                    , placeholder: "Lösenord"
                    , value: Just account.password
                    , onChange: \newpw -> setAccountData _ { password = fromMaybe "" newpw }
                    , label: Just "Lösenord"
                    , validationError: inputFieldErrorMessage $ validateField PasswordField (Just account.password) []
                    }
                ]
            , InputCheckbox.inputCheckbox
                { type_: InputCheckbox.Checkbox
                , name: "sendReset"
                , checked: account.sendReset
                , onChange: \checked -> setAccountData _ { sendReset = checked }
                , label: Just "skicka e-post för återställning av lösenord"
                }
            , DOM.div
                { className: "search--create-account-submit"
                , children:
                    [ DOM.button
                        { type: "submit"
                        , className: "button-green"
                        , disabled: not $ isValid validatedForm
                        , children: [ DOM.text "Skapa konto" ]
                        }
                    ]
                }
            , DOM.div { className: "close-icon", onClick: capture_ cancel }
            ]
        }

    validatedForm :: ValidatedForm NewUserFields User.NewCusnoUser
    validatedForm = case wrapperState of
      AsyncWrapper.Editing state ->
        (\email password -> { cusno: state.cusno
                            , firstName: state.firstName
                            , lastName: state.lastName
                            , email: fromMaybe "" email
                            , password: fromMaybe "" password
                            , consents: state.consents
                            , sendReset: state.sendReset
                            })
        <$> validateField EmailField (Just state.email) []
        <*> validateField PasswordField (Just state.password) []
      _ -> invalid $ pure $ InvalidNotInitialized EmailField

    submit :: ValidatedForm NewUserFields User.NewCusnoUser -> Effect Unit
    submit =
      validation (\errors -> Console.error "Could not create new cusno user.") submitNewAccount

    editError err =
      DOM.div
        { className: "search--error"
        , children:
            [ DOM.div_ [ DOM.text "Något gick fel. Försök igen." ]
            , DOM.div_ [ DOM.text err ]
            ]
        }

renderSetCusno
  :: (Cusno -> Effect Unit)
  -> Effect Unit
  -> ((Maybe Cusno -> Maybe Cusno) -> Effect Unit)
  -> AsyncWrapper.Progress (Maybe Cusno)
  -> JSX
renderSetCusno submitCusno cancel setCusno wrapperState =
  AsyncWrapper.asyncWrapper
    { wrapperState
    , readyView: mempty
    , editingView: render
    , loadingView: const $ DOM.div { className: "tiny-spinner" }
    , successView: const genericSuccess
    , errorView: genericError <<< Just
    }
  where
    render :: Maybe Cusno -> JSX
    render cusno =
      DOM.form
        { className: "search--set-cusno"
        , onSubmit: Events.handler preventDefault $ const $ foldMap submitCusno cusno
        , children:
            [ Grid.row_
                [ InputField.inputField
                    { type_: InputField.Text
                    , name: "cusno"
                    , placeholder: "Kundnummer"
                    , value: Cusno.toString <$> cusno
                    , onChange: \str -> setCusno $ \c -> case str of
                        Nothing -> Nothing
                        Just "" -> Nothing
                        Just s ->
                          (do
                              i <- Int.fromString s
                              guard (i > 0)
                              pure $ Cusno i) <|> c
                    , label: Just "Kundnummer"
                    , validationError: Nothing
                    }
                ]
            , DOM.div
                { className: "search--set-cusno-submit"
                , children:
                    [ DOM.button
                        { type: "submit"
                        , className: "button-green"
                        , disabled: isNothing cusno
                        , children: [ DOM.text "Ändra kundnummer" ]
                        }
                    ]
                }
            , DOM.div { className: "close-icon", onClick: capture_ cancel }
            ]
        }

renderControlPassword :: (String -> Effect Unit) -> Effect Unit -> AsyncWrapper.Progress String -> JSX
renderControlPassword resetPassword cancel wrapperState =
  AsyncWrapper.asyncWrapper
    { wrapperState
    , readyView: mempty
    , editingView: render
    , loadingView: const $ DOM.div { className: "tiny-spinner" }
    , successView: const genericSuccess
    , errorView: const $ genericError Nothing
    }
  where
    render :: String -> JSX
    render email =
      DOM.form
        { className: "search--control-password"
        , onSubmit: Events.handler preventDefault $ const $ resetPassword email
        , children:
            [ Grid.row_
                [ DOM.text $ "E-post: " <> email ]
            , DOM.button
                { type: "submit"
                , className: "button-green"
                , children: [ DOM.text "Skicka e-post för återställning av lösenord" ]
                }
            , DOM.div { className: "close-icon", onClick: capture_ cancel }
            ]
        }

genericError :: Maybe String -> JSX
genericError detail =
  DOM.div
    { className: "error-text"
    , children:
        [ DOM.div_ [ DOM.text "Något gick fel. Försok igen." ]
        ] <> maybe mempty (pure <<< DOM.div_ <<< pure <<< DOM.text) detail
    }

genericSuccess :: JSX
genericSuccess =
  DOM.div
    { children:
        [ DOM.div_ [ DOM.text "Operation lyckades" ] ]
    }
