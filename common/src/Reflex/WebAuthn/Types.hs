{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.WebAuthn.Types where

import qualified Data.Aeson as A
import Data.Aeson.TH
import qualified Data.Text as T

newtype LoginData = LoginData
  { name :: T.Text }
  deriving (Show, A.ToJSON, A.FromJSON)

data Error
  = Error_Frontend FrontendError
  | Error_Backend BackendError
  deriving Eq

data FrontendError
  = FrontendError_NullCredentials
  | FrontendError_CreatePromiseRejected T.Text
  | FrontendError_GetPromiseRejected T.Text
  deriving Eq

newtype BackendError = BackendError T.Text
  deriving (Eq, A.ToJSON, A.FromJSON)

deriveJSON defaultOptions ''Error
deriveJSON defaultOptions ''FrontendError
