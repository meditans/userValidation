{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, ViewPatterns                  #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Reflex
import Reflex.Dom

import Data.String.Conv    (toS)
import Text.Email.Validate (toByteString, validate, EmailAddress(..))

--------------------------------------------------------------------------------

{- Note: The structure of the application

As you can see the structure of the main is quite linear, I just create the
widgets to query the user for the informations I care about, and it all is taken
care by the validateInput function

The htmlHead function provides some styling from a cdn for ease of use.
-}

htmlHead = do
  styleSheet "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.1.0/milligram.min.css"
  where
    styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

main = mainWidgetWithHead htmlHead $ do
  el "h1" (text "A validation demo")
  rec firstName <- validateInput "First Name:" nameValidation  signUpButton
      lastName  <- validateInput "Last Name:"  nameValidation  signUpButton
      mail      <- validateInput "Email:"      emailValidation signUpButton
      age       <- validateInput "Age:"        ageValidation   signUpButton
      signUpButton <- button "Sign up"
  let user = (liftM4 . liftM4) User firstName lastName mail age
  notifyLogin user signUpButton

--------------------------------------------------------------------------------
-- Data modeling

data User = User { firstName :: Text
                 , lastName :: Text
                 , email :: EmailAddress
                 , age :: Int
                 } deriving (Eq, Show)

prettyPrint :: User -> Text
prettyPrint u = unwords [ firstName u , lastName u
                        , parenthesized (toS . toByteString $ email u)]
  where
    parenthesized s = "(" <> s <> ")"

--------------------------------------------------------------------------------

{- Note: A general validation function

The validateInput function takes as parameters: the prompt used to ask the user
for a specific information, a pure validation function and an event to
syncronize the update with (in this example the signUpButton - note that with
less effort the output could be always updated instantly, but in this case it
didn't fell right from an UI perspective). The function is further commented in
the code below:
-}

type Prompt = Text

validateInput :: MonadWidget t m
              => Prompt -> (Text -> Either Text a) -> Event t b -> m (Dynamic t (Maybe a))
validateInput prompt pureValidation event = do
  -- 1) Declaring the graphical interface: the prompt and the input field
  text prompt
  inputField <- textInput def
  -- 2) Using the pure validation function to construct the hidden property of
  -- the feedback label and error message (both dynamic)
  let queryResult = fmap pureValidation (_textInput_value inputField)
      dynAttrs = either (const mempty) (const hidden) <$> queryResult
      dynError = either id             (const "")     <$> queryResult
  -- 3) Freezing the events in order to display the update only when the button
  -- is pressed
  frozenAttrs <- resampleOn event hidden dynAttrs
  frozenError <- resampleOn event "" dynError
  -- 4) Optionally showing a label containing the eventual error
  elDynAttr "p" frozenAttrs (dynText frozenError)
  -- Return the actual content of the query for further processing
  return $ fmap (either (const Nothing) Just) queryResult

--------------------------------------------------------------------------------

notifyLogin :: MonadWidget t m
            => Dynamic t (Maybe User) -> Event t b -> m ()
notifyLogin user event = do
  hiddenAttr  <- resampleOn event hidden (maybe hidden (const mempty) <$> user)
  loginReport <- resampleOn event ""     (maybe ""     prettyPrint    <$> user)
  elDynAttr "h5" hiddenAttr (do text "User "; dynText loginReport; text " logged in")

--------------------------------------------------------------------------------

{- Note: Validation with pure functions

Down below there are some pure functions used for validation: they all take the
text from the input field as a parameter, and return either an error to be
displayed, or the value correctly parsed. This ensures the complete separation
between the validation logic and the presentation of the application.
-}

ageValidation :: Text -> Either Text Int
ageValidation (readMay -> Just age :: Maybe Int)
  | 18 <= age && age <= 120 = Right age
  | age <= 18               = Left "You must be at least 18 years old."
  | 120 <= age              = Left "No way. Try to pick a reasonable fake age."
ageValidation (readMay -> Nothing :: Maybe Int)
                            = Left "Please enter your age."

nameValidation :: Text -> Either Text Text
nameValidation "" = Left "Please enter a non-empty name."
nameValidation n  = Right n

emailValidation :: Text -> Either Text EmailAddress
emailValidation (validate . toS -> Right addr) = Right addr
emailValidation (validate . toS -> Left _)     = Left "Please enter your email address."

--------------------------------------------------------------------------------
-- Utilities

-- A commonly used property in this demo
hidden :: Map Text Text
hidden = "hidden" =: "true"

resampleOn :: (MonadWidget t m) => Event t b -> a -> Dynamic t a -> m (Dynamic t a)
resampleOn event initial dyn = holdDyn initial (tag (current dyn) event)
