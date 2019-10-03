module KSF.ValidatableForm where

import Prelude

import Data.Array (find)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList, head)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup (V, andThen, invalid, unV)

class ValidatableField a where
  validateField :: a -> Maybe String -> ValidatedForm a (Maybe String)

type ValidatedForm a b = V (NonEmptyList (ValidationError a)) b

data ValidationError a
  = Invalid a String
  | InvalidEmpty a String
  | InvalidPatternFailure a String
  | InvalidEmailInUse a String
  | InvalidNotInitialized a -- Fictional state only to be set when the form is first rendered

validateForm :: forall a b. ValidatableField b => (a -> ValidatedForm b a) -> a -> ValidatedForm b a
validateForm f = f

validateZipCode :: forall a. a -> Maybe String -> ValidatedForm a (Maybe String)
validateZipCode field zipCode =
  validateEmptyField field "Postnummer krävs." zipCode `andThen`
  validateInputWithRegex field "^[\\s|\\w|-]+$" "Postnummerfältet kan bara innehålla siffror och bokstäver."

validateEmailAddress :: forall a. Eq a => a -> Maybe String -> Array (ValidationError a) -> ValidatedForm a (Maybe String)
validateEmailAddress emailField email serverErrors =
  validateServerError emailField serverErrors email `andThen`
  validateEmptyField emailField "E-postadress krävs." `andThen`
  validateInputWithRegex emailField emailRegex "Ogiltig E-postadress."
  where
    -- From https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#Basic_validation
    emailRegex = "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

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

-- | NOTE: Even though `val` is not required by this function, it must be taken in and returned as is,
--   in order to keep chaining validation functions working.
validateServerError ::  forall a. Eq a => a -> Array (ValidationError a) -> Maybe String -> ValidatedForm a (Maybe String)
validateServerError fieldName serverErrors val =
  case find ((_ == fieldName) <<< validationInputFieldOf) serverErrors of
    Just err -> invalid $ pure err
    Nothing  -> pure $ val

notInitialized :: forall a. a -> ValidatedForm a (Maybe String)
notInitialized field = invalid $ pure $ InvalidNotInitialized field

isValidOrNotInitialized :: forall a. ValidatedForm a (Maybe String) -> Boolean
isValidOrNotInitialized = isNothing <<< inputFieldErrorMessage

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
inputFieldErrorMessage = unV handleInvalidField (\_ -> Nothing)
  where
    handleInvalidField errs
      -- If field is not initialized, it's considered to be valid
      | (InvalidNotInitialized _) <- head errs = Nothing
      | otherwise = Just $ validationErrorMessageOf $ head errs
