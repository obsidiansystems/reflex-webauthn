{-|
Module      : Reflex.WebAuthn.Route
Description : Obelisk-style routes for the reflex-webauthn-* packages
Copyright   : (c) Obsidian Systems, 2022

This module provides Obelisk-style routes for the reflex-webauthn-* packages.

__TIP__: Use 'Reflex.WebAuthn.Route.webauthnRouteEncoder' to setup webauthn routes on the backend easily.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.WebAuthn.Route where

import Control.Monad.Error.Class (MonadError)
import Data.Text (Text)
import Data.Universe

import Obelisk.Route
import Obelisk.Route.TH

data RegisterRoute
  = RegisterRoute_Begin
  | RegisterRoute_Complete
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe RegisterRoute

registerRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse RegisterRoute PageName
registerRouteEncoder = enumEncoder $ \case
  RegisterRoute_Begin -> (["begin"], mempty)
  RegisterRoute_Complete -> (["complete"], mempty)

data LoginRoute
  = LoginRoute_Begin
  | LoginRoute_Complete
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe LoginRoute

loginRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse LoginRoute PageName
loginRouteEncoder = enumEncoder $ \case
  LoginRoute_Begin -> (["begin"], mempty)
  LoginRoute_Complete -> (["complete"], mempty)

data WebAuthnRoute :: * -> * where
  WebAuthnRoute_Login :: WebAuthnRoute LoginRoute
  WebAuthnRoute_Register :: WebAuthnRoute RegisterRoute

deriveRouteComponent ''WebAuthnRoute

-- | The 'Encoder' for the 'WebAuthnRoute' route. This should be used by the client app's backend
-- route encoder.
--
-- This function generates URLs as follows:
--
-- > webauthn
-- > |---- register
-- > |     |----- begin
-- > |     |----- complete
-- > |---- login
-- >       |----- begin
-- >       |----- complete
--
-- For instance, there will be a URL at @\/webauthn\/register\/begin@, that corresponds to the registration workflow, and so on.
webauthnRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (R WebAuthnRoute) PageName
webauthnRouteEncoder = pathComponentEncoder $ \case
  WebAuthnRoute_Register -> PathSegment "register" registerRouteEncoder
  WebAuthnRoute_Login -> PathSegment "login" loginRouteEncoder
