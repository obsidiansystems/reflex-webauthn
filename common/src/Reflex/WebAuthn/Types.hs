{-|
Module      : Reflex.WebAuthn.Types
Description : Data types for the reflex-webauthn-* packages
Copyright   : (c) Obsidian Systems, 2022

This module provides Data types for the reflex-webauthn-* packages.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.WebAuthn.Types where

import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.List.NonEmpty
import qualified Data.Text as T

import Reflex.Dom.Xhr

-- | Used to represent username
newtype LoginData = LoginData
  { name :: T.Text }
  deriving (Show, A.ToJSON, A.FromJSON)

-- | Parent type of all the errors
data Error
  = Error_Frontend FrontendError  -- ^ Represents errors occurring on the client
  | Error_Backend BackendError    -- ^ Represents errors occurring on the server
  | Error_Xhr XhrError            -- ^ Represents XHR related errors
  deriving (Eq, Show)

-- | Represents all frontend errors
data FrontendError
  = FrontendError_NullCredentials               -- ^ Received @null@ on the browser,
                                                -- while trying to run [__create__](https://developer.mozilla.org/en-US/docs/Web/API/CredentialsContainer/create)
                                                -- or [__get__](https://developer.mozilla.org/en-US/docs/Web/API/CredentialsContainer/get)
  | FrontendError_CreatePromiseRejected T.Text  -- ^ The browser rejected the promise returned by [__create__](https://developer.mozilla.org/en-US/docs/Web/API/CredentialsContainer/create)
  | FrontendError_GetPromiseRejected T.Text     -- ^ The browser rejected the promise returned by [__get__](https://developer.mozilla.org/en-US/docs/Web/API/CredentialsContainer/get)
  | FrontendError_JsonParseSyntaxError          -- ^ [__JSON.parse__](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse) threw a SyntaxError.
  | FrontendError_BrowserNotSupported           -- ^ The browser does not support WebAuthn.
  | FrontendError_PropertyMissing T.Text        -- ^ A required property was missing on an object received from the backend.
  deriving (Eq, Show)

-- Represents all request errors
data XhrError
  = XhrError_Exception XhrException             -- ^ The request itself failed, with an 'XhrException'
  | XhrError_NoDataInResponse                   -- ^ Expected some data in response, but didn't receive any.
  deriving (Eq, Show)

-- Represents all backend errors
data BackendError
  = BackendError_CouldNotReadData T.Text                  -- ^ Unable to read data sent from the client
  | BackendError_DbError DbError                          -- ^ Encountered a DB error
  | BackendError_RegistrationFailed (NonEmpty T.Text)     -- ^ Registration workflow failed, with a nonempty list of errors
  | BackendError_AuthenticationFailed (NonEmpty T.Text)   -- ^ Authentication workflow failed, with a nonempty list of errors
  | BackendError_SignatureCounterPotentiallyCloned        -- ^ Signature Counter was potentially cloned
  deriving (Eq, Show)

-- Represents all DB errors
data DbError
  = DbError_UserAlreadyExists             -- ^ Error during registration, a user with the same name already exists
  | DbError_UserDoesNotExist              -- ^ Error during authentication, user with the given name does not exist
  | DbError_CredentialEntryDoesNotExist   -- ^ Credential entry does not exist for the user
  deriving (Eq, Show)


deriveJSON defaultOptions ''Error
deriveJSON defaultOptions ''FrontendError
deriveJSON defaultOptions ''XhrException
deriveJSON defaultOptions ''XhrError
deriveJSON defaultOptions ''BackendError
deriveJSON defaultOptions ''DbError
