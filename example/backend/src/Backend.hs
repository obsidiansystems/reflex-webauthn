{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Obelisk.Route.Frontend
import Obelisk.Backend
import Reflex.WebAuthn.Backend

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = withWebAuthnBackend $ \webAuthnRouteHandler -> \case
      BackendRoute_Missing :/ () -> pure ()
      BackendRoute_WebAuthn :/ webAuthnRoute -> webAuthnRouteHandler webAuthnRoute
  , _backend_routeEncoder = fullRouteEncoder
  }
