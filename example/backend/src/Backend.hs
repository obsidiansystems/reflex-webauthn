{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Obelisk.Route.Frontend
import Obelisk.Backend
import Reflex.WebAuthn.Backend
import qualified Crypto.WebAuthn as WA

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = withWebAuthnBackend $ \webAuthnRouteHandler -> \case
      BackendRoute_Missing :/ () -> pure ()
      BackendRoute_WebAuthn :/ webAuthnRoute -> webAuthnRouteHandler modifyRegCredOpts modifyLoginCredOpts webAuthnRoute
  , _backend_routeEncoder = fullRouteEncoder
  }
  where
    modifyRegCredOpts :: ModifyCredentialOptionsRegistration
    modifyRegCredOpts regCredOpts = regCredOpts
      { WA.corRp =
        (WA.corRp regCredOpts)
          { WA.creName = "Example"
          }
      }

    modifyLoginCredOpts :: ModifyCredentialOptionsAuthentication
    modifyLoginCredOpts loginCredOpts = loginCredOpts { WA.coaTimeout = Just $ WA.Timeout 10000 }