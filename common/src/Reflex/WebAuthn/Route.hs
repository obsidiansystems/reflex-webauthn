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

-- | Routes for WebAuthn Registration workflow
data RegisterRoute
  = RegisterRoute_Begin     -- ^ Registration Begin, where we provide Credential Options to user
  | RegisterRoute_Complete  -- ^ Registration Complete, user's credentials have now been stored
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe RegisterRoute

-- | The 'Encoder' for the 'RegisterRoute' route.
registerRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse RegisterRoute PageName
registerRouteEncoder = enumEncoder $ \case
  RegisterRoute_Begin -> (["begin"], mempty)
  RegisterRoute_Complete -> (["complete"], mempty)

-- | Routes for WebAuthn Authentication workflow
data LoginRoute
  = LoginRoute_Begin      -- ^ Authentication Begin, where we provide Credential Options to user
  | LoginRoute_Complete   -- ^ Authentication Complete, user has now been authenticated
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe LoginRoute

-- | The 'Encoder' for the 'LoginRoute' route.
loginRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse LoginRoute PageName
loginRouteEncoder = enumEncoder $ \case
  LoginRoute_Begin -> (["begin"], mempty)
  LoginRoute_Complete -> (["complete"], mempty)

-- | Routes for webauthn, includes both registration and authentication (login)
data WebAuthnRoute :: * -> * where
  WebAuthnRoute_Login :: WebAuthnRoute LoginRoute         -- ^ Registration Routes
  WebAuthnRoute_Register :: WebAuthnRoute RegisterRoute   -- ^ Login Routes

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
