{-|
Module      : Reflex.WebAuthn.Backend
Description : Provides functions for setting up a relying party server for WebAuthn
Copyright   : (c) Obsidian Systems, 2022

This module provides a backend for Obelisk based WebAuthn projects.
It has functions that allow developers to customize Credential Options that will be sent to the client.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.WebAuthn.Backend(
  ModifyCredentialOptionsRegistration,
  ModifyCredentialOptionsAuthentication,
  WebAuthnRouteHandler,
  ServeRouteHandler,
  RunBackend,
  WebAuthnBackendHandler,
  withWebAuthnBackend
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash (hash)
import qualified Crypto.WebAuthn as WA
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.List.NonEmpty as NL
import qualified Data.Map.Strict as M
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Validation
import Snap.Internal.Core
import Snap.Extras.JSON
import Time.System

import Database.Beam.Postgres
import Gargoyle.PostgreSQL.Connect
import Obelisk.Route.Frontend

import Reflex.WebAuthn.Types
import Reflex.WebAuthn.Route
import Reflex.WebAuthn.DB.DB

finishWithError :: (MonadSnap m) => BackendError -> m a
finishWithError err = do
  writeLBS $ A.encode err
  getResponse >>= finishWith

-- sendData :: (MonadSnap m, A.ToJSON a) => a -> m ()
-- sendData = writeLBS . A.encode . toRight
--   where
--     toRight :: a -> Either String a
--     toRight = Right

defaultRegistrationOptions :: T.Text -> WA.Challenge -> IO (WA.CredentialOptions 'WA.Registration)
defaultRegistrationOptions userName challenge = do
  userHandle <- liftIO $ WA.generateUserHandle
  let
    userEntity =
      WA.CredentialUserEntity
        { WA.cueId = userHandle,
          WA.cueDisplayName = WA.UserAccountDisplayName userName,
          WA.cueName = WA.UserAccountName userName
        }
  pure $ WA.CredentialOptionsRegistration
    { WA.corRp = WA.CredentialRpEntity {WA.creId = Nothing, WA.creName = ""},
      WA.corUser = userEntity,
      WA.corChallenge = challenge,
      WA.corPubKeyCredParams =
        [ WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey,
              WA.cpAlg = WA.CoseAlgorithmES256
            },
          WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey,
              WA.cpAlg = WA.CoseAlgorithmRS256
            }
        ],
      WA.corTimeout = Nothing,
      WA.corExcludeCredentials = [],
      WA.corAuthenticatorSelection =
        Just
          WA.AuthenticatorSelectionCriteria
            { WA.ascAuthenticatorAttachment = Nothing,
              WA.ascResidentKey = WA.ResidentKeyRequirementDiscouraged,
              WA.ascUserVerification = WA.UserVerificationRequirementPreferred
            },
      WA.corAttestation = WA.AttestationConveyancePreferenceDirect,
      WA.corExtensions = Nothing
    }

defaultAuthenticationOptions :: WA.Challenge -> [WA.CredentialEntry] -> WA.CredentialOptions 'WA.Authentication
defaultAuthenticationOptions challenge credentials = WA.CredentialOptionsAuthentication
  { WA.coaRpId = Nothing
  , WA.coaTimeout = Nothing
  , WA.coaChallenge = challenge
  , WA.coaAllowCredentials = map mkCredentialDescriptor credentials
  , WA.coaUserVerification = WA.UserVerificationRequirementPreferred
  , WA.coaExtensions = Nothing
  }

writeOptionsToMVar :: WA.Challenge -> a -> MVar (M.Map WA.Challenge a) -> IO ()
writeOptionsToMVar challenge opts optsMVarMap = do
  optsMap <- takeMVar optsMVarMap
  putMVar optsMVarMap $ M.insert challenge opts optsMap

webAuthnRouteHandler
  :: (MonadSnap m)
  => Pool Connection
  -> MVar (M.Map WA.Challenge (WA.CredentialOptions 'WA.Registration))
  -> MVar (M.Map WA.Challenge (WA.CredentialOptions 'WA.Authentication))
  -> WA.Origin
  -> WA.RpIdHash
  -> ModifyCredentialOptionsRegistration
  -> ModifyCredentialOptionsAuthentication
  -> R WebAuthnRoute
  -> m ()
webAuthnRouteHandler pool registerOptionMapVar loginOptionMapVar origin rpIdHash modifyRegCredOpts modifyLoginCredOpts = \case
  WebAuthnRoute_Register :/ registerRoute -> case registerRoute of
    RegisterRoute_Begin -> do
      loginDataEither <- getJSON
      case loginDataEither of
        Left err -> finishWithError $ BackendError_CouldNotReadData $ "username: " <> T.pack err
        Right (LoginData userName) -> do
          -- Check if there already is a user by this name
          userExists <- liftIO $ checkIfUserExists pool userName
          when userExists $ finishWithError $ BackendError_DbError DbError_UserAlreadyExists
          challenge <- liftIO WA.generateChallenge
          credOpts <- liftIO $ modifyRegCredOpts <$> defaultRegistrationOptions userName challenge
          liftIO $ writeOptionsToMVar challenge credOpts registerOptionMapVar
          writeLBS $ A.encode $ WA.encodeCredentialOptionsRegistration credOpts

    RegisterRoute_Complete -> do
      dateTime <- liftIO dateCurrent

      credential <- first T.pack <$> getJSON
      cred <- case credential >>= WA.decodeCredentialRegistration WA.allSupportedFormats of
        Left err -> finishWithError $ BackendError_CouldNotReadData err
        Right result -> pure result

      let challenge = WA.ccdChallenge $ WA.arrClientData $ WA.cResponse cred
      registerOptionMap <- liftIO $ takeMVar registerOptionMapVar
      forM_ (M.lookup challenge registerOptionMap) $ \credOpts -> do
        case WA.verifyRegistrationResponse origin rpIdHash mempty dateTime credOpts cred of
          Failure nonEmptyErrorList -> finishWithError $ BackendError_RegistrationFailed $ NL.map (T.pack . show) nonEmptyErrorList
          Success registrationResponse -> do
            liftIO $ do
              putMVar registerOptionMapVar $ M.delete challenge registerOptionMap
              insertUser pool $ WA.corUser credOpts
              insertCredentialEntry pool $ WA.rrEntry registrationResponse
            writeLBS "You have registered successfully"

  WebAuthnRoute_Login :/ loginRoute -> case loginRoute of
    LoginRoute_Begin -> do
      loginDataEither <- getJSON
      case loginDataEither of
        Left err -> finishWithError $ BackendError_CouldNotReadData $ "username: " <> T.pack err
        Right (LoginData username) -> do
          liftIO $ print username
          credentials <- liftIO $ getCredentialEntryByUser pool username
          when (null credentials) $ finishWithError $ BackendError_DbError DbError_UserDoesNotExist

          challenge <- liftIO WA.generateChallenge
          let credOpts = modifyLoginCredOpts $ defaultAuthenticationOptions challenge credentials
          liftIO $ writeOptionsToMVar challenge credOpts loginOptionMapVar
          writeLBS $ A.encode $ WA.encodeCredentialOptionsAuthentication credOpts

    LoginRoute_Complete -> do
      credential <- first T.pack <$> getJSON
      cred <- case credential >>= WA.decodeCredentialAuthentication of
        Left err -> finishWithError $ BackendError_CouldNotReadData err
        Right result -> pure result

      entryMaybe <- liftIO $ getCredentialEntryByCredentialId pool $ WA.cIdentifier cred
      entry <- case entryMaybe of
        Nothing -> finishWithError $ BackendError_DbError DbError_CredentialEntryDoesNotExist
        Just entry -> pure entry

      let challenge = WA.ccdChallenge $ WA.araClientData $ WA.cResponse cred
      loginOptionMap <- liftIO $ takeMVar loginOptionMapVar
      forM_ (M.lookup challenge loginOptionMap) $ \credOpts -> do
        liftIO $ putMVar loginOptionMapVar $ M.delete challenge loginOptionMap
        WA.AuthenticationResult newSigCount <- case WA.verifyAuthenticationResponse origin rpIdHash (Just (WA.ceUserHandle entry)) entry credOpts cred of
          Failure nonEmptyErrorList -> finishWithError $ BackendError_AuthenticationFailed $ NL.map (T.pack . show) nonEmptyErrorList
          Success result -> pure result
        case newSigCount of
          WA.SignatureCounterZero -> writeLBS "You were logged in."
          WA.SignatureCounterUpdated counter -> do
            liftIO $ updateSignatureCounter pool (WA.cIdentifier cred) counter
            writeLBS "You were logged in."
          WA.SignatureCounterPotentiallyCloned -> finishWithError BackendError_SignatureCounterPotentiallyCloned

-- | Type of a function that allows us to modify a Registration Credential Option.
type ModifyCredentialOptionsRegistration = WA.CredentialOptions 'WA.Registration -> WA.CredentialOptions 'WA.Registration
-- | Type of a function that allows us to modify an Authentication Credential Option.
type ModifyCredentialOptionsAuthentication = WA.CredentialOptions 'WA.Authentication -> WA.CredentialOptions 'WA.Authentication

-- | Type of a function that handles 'WebAuthnRoute' routes in the Snap Monad.
type WebAuthnRouteHandler
  = ModifyCredentialOptionsRegistration     -- ^ Function to customize __Registration__ Credential Options
  -> ModifyCredentialOptionsAuthentication  -- ^ Function to customize __Authentication__ Credential Options
  -> R WebAuthnRoute                        -- ^ The route to match.
  -> Snap ()
-- | Type of a function that handles any route in the Snap Monad.
type ServeRouteHandler route = R route -> Snap ()
-- | Type of a function that can be passed to 'Obelisk.Backend._backend_run'.
type RunBackend route = ((R route -> Snap ()) -> IO ()) -> IO ()

-- | A function that takes a function that handles 'WebAuthnRoute' routes, and returns a function that serves all routes in the 'Snap' Monad.
type WebAuthnBackendHandler route
  = WebAuthnRouteHandler                   -- ^ Function to handle 'WebAuthnRoute' routes
  -> ServeRouteHandler route

-- | This function can be used as a drop-in replacement for 'Obelisk.Backend._backend_run'.
--
-- @
-- backend :: Backend BackendRoute FrontendRoute
-- backend = Backend
--   { _backend_run = withWebAuthnBackend $ \webAuthnRouteHandler -> \case
--       ... -- Other routes
--       BackendRoute_WebAuthn :/ webAuthnRoute -> webAuthnRouteHandler modifyRegCredOpts modifyLoginCredOpts webAuthnRoute
--   , _backend_routeEncoder = ...
--   }
-- @
--
-- Here @modifyRegCredOpts@ and @modifyLoginCredOpts@ are functions for modifying registration and authentication credential options respectively.
--
-- Essentially, this function takes a function, and returns a function. The returned function can be used directly for 'Obelisk.Backend._backend_run'.
withWebAuthnBackend
  :: WebAuthnBackendHandler backendRoute       -- ^ User provided function, takes 1 argument, which is also a function
  -> RunBackend backendRoute
withWebAuthnBackend routeMatcher =
  \serve ->
    withDb "db" $ \pool -> do

      -- Initialise the database
      initDb pool

      registerOptionMapVar <- newMVar mempty :: IO (MVar (M.Map WA.Challenge (WA.CredentialOptions 'WA.Registration)))
      loginOptionMapVar <- newMVar mempty :: IO (MVar (M.Map WA.Challenge (WA.CredentialOptions 'WA.Authentication)))

      let
        origin = WA.Origin "http://localhost:8000"
        rpIdHash = WA.RpIdHash $ hash $ T.encodeUtf8 "localhost"
      serve (routeMatcher $ webAuthnRouteHandler pool registerOptionMapVar loginOptionMapVar origin rpIdHash)

mkCredentialDescriptor :: WA.CredentialEntry -> WA.CredentialDescriptor
mkCredentialDescriptor credEntry =
  WA.CredentialDescriptor
    { WA.cdTyp = WA.CredentialTypePublicKey,
      WA.cdId = WA.ceCredentialId credEntry,
      WA.cdTransports = Just $ WA.ceTransports credEntry
    }
