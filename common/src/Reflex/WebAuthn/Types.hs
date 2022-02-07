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

newtype LoginData = LoginData
  { name :: T.Text }
  deriving (Show, A.ToJSON, A.FromJSON)

data Error
  = Error_Frontend FrontendError
  | Error_Backend BackendError
  | Error_Xhr XhrError
  deriving (Eq, Show)

data FrontendError
  = FrontendError_NullCredentials
  | FrontendError_CreatePromiseRejected T.Text
  | FrontendError_GetPromiseRejected T.Text
  | FrontendError_JsonParseSyntaxError
  | FrontendError_BrowserNotSupported
  | FrontendError_PropertyMissing T.Text
  deriving (Eq, Show)

data XhrError
  = XhrError_Exception XhrException
  | XhrError_NoResponse
  deriving (Eq, Show)

data BackendError
  = BackendError_CouldNotReadData T.Text
  | BackendError_DbError DbError
  | BackendError_RegistrationFailed (NonEmpty T.Text)
  | BackendError_AuthenticationFailed (NonEmpty T.Text)
  | BackendError_CredentialEntryDoesNotExist
  | BackendError_SignatureCounterCloned
  deriving (Eq, Show)

data DbError
  = DbError_UserAlreadyExists
  | DbError_UserDoesNotExist
  deriving (Eq, Show)


deriveJSON defaultOptions ''Error
deriveJSON defaultOptions ''FrontendError
deriveJSON defaultOptions ''XhrException
deriveJSON defaultOptions ''XhrError
deriveJSON defaultOptions ''BackendError
deriveJSON defaultOptions ''DbError
