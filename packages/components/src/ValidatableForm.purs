module KSF.ValidatableForm where

import Prelude

import Data.Array (filter, find)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.List.NonEmpty (NonEmptyList, head)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (length, null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup (V, andThen, invalid, toEither, validation)
import Effect (Effect)

class ValidatableField a where
  validateField :: a -> Maybe String -> Array (ValidationError a) -> ValidatedForm a (Maybe String)

type ValidatedForm a b = V (NonEmptyList (ValidationError a)) b

data ValidationError a
  = Invalid a String
  | InvalidEmpty a String
  | InvalidPatternFailure a String
  | InvalidEmailInUse a String
  | InvalidNotInitialized a -- Fictional state only to be set when the form is first rendered

type CommonContactInformation =
  { firstName     :: Maybe String
  , lastName      :: Maybe String
  , streetAddress :: Maybe String
  , city          :: Maybe String
  , zipCode       :: Maybe String
  , countryCode   :: Maybe String
  }

data CommonContactInformationFormField
  = FirstName
  | LastName
  | StreetAddress
  | City
  | Zip
  | Country

derive instance eqCommonContactInformationFormField :: Eq CommonContactInformationFormField
instance validatableFieldRegistrationFormField :: ValidatableField CommonContactInformationFormField where
  validateField field value serverErrors = case field of
    FirstName     -> validateEmptyField FirstName     "Förnamn krävs."   value
    LastName      -> validateEmptyField LastName      "Efternamn krävs." value
    StreetAddress -> validateEmptyField StreetAddress "Adress krävs."    value
    City          -> validateEmptyField City          "Stad krävs."      value
    Country       -> validateEmptyField Country       "Land krävs."      value
    Zip           -> validateZipCode field value

emptyCommonContactInformation :: CommonContactInformation
emptyCommonContactInformation =
  { firstName: Nothing
  , lastName: Nothing
  , streetAddress: Nothing
  , city: Nothing
  , zipCode: Nothing
  , countryCode: Nothing
  }

validateForm :: forall a b. ValidatedForm a b -> (Either (NonEmptyList (ValidationError a)) b -> Effect Unit) -> Effect Unit
validateForm form callback = validation (callback <<< Left) (callback <<< Right) form

isFormInvalid :: forall a b. ValidatedForm a b -> Boolean
isFormInvalid form
  | Left errs <- toEither form
  = not $ all isNotInitialized $ errs
  | otherwise = false

noValidation :: forall a. Maybe String -> ValidatedForm a (Maybe String)
noValidation v = pure v

validateZipCode :: forall a. a -> Maybe String -> ValidatedForm a (Maybe String)
validateZipCode field zipCode =
  validateEmptyField field "Postnummer krävs." zipCode `andThen`
  validateInputWithRegex field "^[\\s|\\w|-]+$" "Postnummerfältet kan bara innehålla siffror och bokstäver."

validateEmailAddress :: forall a. Eq a => a -> Maybe String -> ValidatedForm a (Maybe String)
validateEmailAddress emailField email =
  validateEmptyField emailField "E-postadress krävs." email `andThen`
  validateInputWithRegex emailField emailRegex "Ogiltig E-postadress."
  where
    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

validatePhone :: forall a. a -> Maybe String -> ValidatedForm a (Maybe String)
validatePhone field phone =
  validateEmptyField field "Telefon krävs." phone `andThen`
  validateInputWithRegex field "^[\\d|\\+|\\s|-|\\(|\\)]+$" "Telefonnummer kan bara bestå av siffror, mellanslag och +-tecken."


validatePassword :: forall a. a -> Maybe String -> ValidatedForm a (Maybe String)
validatePassword field password =
  validateEmptyField field "Lösenord krävs." password `andThen`
  validatePasswordLength field

validatePasswordLength :: forall a. a -> Maybe String -> ValidatedForm a (Maybe String)
validatePasswordLength field Nothing = notInitialized field
validatePasswordLength field password
  | Just pw <- password, length pw >= 6 = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid field "Lösenordet måste ha minst 6 tecken."

validatePasswordComparison :: forall a. a -> a -> Maybe String -> Maybe String -> ValidatedForm a (Maybe String)
validatePasswordComparison originalField _confirmField Nothing Nothing = notInitialized originalField
validatePasswordComparison originalField confirmField password confirmedPassword
  | Just pw <- password
  , Just confirmedPw <- confirmedPassword
  , pw == confirmedPw
  = pure $ Just pw
  | otherwise = invalid $ pure $ Invalid confirmField "Lösenorden överensstämmer inte med varandra."

validateEmptyField :: forall a. a -> String -> Maybe String -> ValidatedForm a (Maybe String)
validateEmptyField field _ Nothing = notInitialized field
validateEmptyField fieldName validationErr (Just value) =
  if null value
    then invalid $ pure $ InvalidEmpty fieldName validationErr
    else pure (Just value)

validateInputWithRegex :: forall a. a -> String -> String -> Maybe String -> ValidatedForm a (Maybe String)
validateInputWithRegex fieldName regexString errMsg inputValue
  | Just val <- inputValue
  , Right regexPattern <- Regex.regex regexString Regex.Flags.noFlags
  , Regex.test regexPattern val
  = pure $ Just val
  | otherwise = invalid $ pure $ InvalidPatternFailure fieldName errMsg

validateWithServerErrors
 :: forall a. Eq a
 => Array (ValidationError a)
 -> a
 -> Maybe String
 -> (a -> Maybe String -> ValidatedForm a (Maybe String))
 -> ValidatedForm a (Maybe String)
validateWithServerErrors serverErrors field value validationFn =
  validateServerError field serverErrors value `andThen` (validationFn field)

-- | NOTE: Even though `val` is not required by this function, it must be taken in and returned as is,
--   in order to keep chaining validation functions working.
validateServerError ::  forall a. Eq a => a -> Array (ValidationError a) -> Maybe String -> ValidatedForm a (Maybe String)
validateServerError fieldName serverErrors val =
  case find ((_ == fieldName) <<< validationInputFieldOf) serverErrors of
    Just err -> invalid $ pure err
    Nothing  -> pure $ val

removeServerErrors :: forall a. Eq a => a -> Array (ValidationError a) -> Array (ValidationError a)
removeServerErrors field serverErrors = filter ((field /= _) <<< validationInputFieldOf) serverErrors

notInitialized :: forall a. a -> ValidatedForm a (Maybe String)
notInitialized field = invalid $ pure $ InvalidNotInitialized field

isValidOrNotInitialized :: forall a. ValidatedForm a (Maybe String) -> Boolean
isValidOrNotInitialized = isNothing <<< inputFieldErrorMessage

isNotInitialized :: forall a. ValidationError a -> Boolean
isNotInitialized (InvalidNotInitialized _) = true
isNotInitialized _ = false

validationErrorMessageOf :: forall a. ValidationError a -> String
validationErrorMessageOf = case _ of
  Invalid _ err               -> err
  InvalidEmpty _ err          -> err
  InvalidPatternFailure _ err -> err
  InvalidEmailInUse _ err     -> err
  InvalidNotInitialized _     -> "NotInitialized"

validationInputFieldOf :: forall a. ValidationError a -> a
validationInputFieldOf e = case e of
  Invalid f _               -> f
  InvalidEmpty f _          -> f
  InvalidPatternFailure f _ -> f
  InvalidEmailInUse f _     -> f
  InvalidNotInitialized f   -> f

inputFieldErrorMessage :: forall a. ValidatedForm a (Maybe String) -> Maybe String
inputFieldErrorMessage = validation handleInvalidField (\_ -> Nothing)
  where
    handleInvalidField errs
      -- If field is not initialized, it's considered to be valid
      | (InvalidNotInitialized _) <- head errs = Nothing
      | otherwise = Just $ validationErrorMessageOf $ head errs
