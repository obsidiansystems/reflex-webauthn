{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Reflex.WebAuthn.Frontend(
  setupLoginWorkflow,
  setupRegisterWorkflow
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle
import Reflex.Dom.Core hiding (Error)

import Reflex.WebAuthn.Types


s :: String -> String
s = id

consoleLog :: (MonadJSM m, ToJSVal a0) => a0 -> m ()
consoleLog t = liftJSM $ do
  console <- jsg $ s "console"
  void $ console ^. js1 (s "log") t

fromBase64UrlString :: ToJSVal value => value -> JSM (Either String (JSM JSVal))
fromBase64UrlString strVal = do
  bstrEither <- B64.decode . T.encodeUtf8 <$> valToText strVal
  pure $ bstrEither <&> \bstr -> do
    jsArray <- toJSValListOf $ B.unpack $ B.fromStrict bstr
    new (jsg $ s "Uint8Array") jsArray

decodeBase64Property :: (MonadJSM m) => Object -> String -> m ()
decodeBase64Property someObj propName = liftJSM $ do
  propVal <- someObj ^. js propName
  eitherBuffer <- fromBase64UrlString propVal
  forM_ eitherBuffer $ \buf ->
    objSetPropertyByName someObj propName buf

toBase64UrlString :: MakeArgs args => args -> JSM T.Text
toBase64UrlString propVal = do
  uint8Array <- new (jsg $ s "Uint8Array") propVal
  bytes <- B.toStrict . B.pack <$> fromJSValUncheckedListOf uint8Array
  pure $ T.decodeUtf8 $ B64.encode bytes

copyProperty :: MonadJSM m => Object -> Object -> String -> m ()
copyProperty = copyPropertyWithModification pure

copyPropertyWithModification :: (ToJSVal a, MonadJSM m) => (JSVal -> JSM a) -> Object -> Object -> String -> m ()
copyPropertyWithModification f oldObj newObj propName = liftJSM $ do
  propVal <- objGetPropertyByName oldObj propName
  isPropNull <- ghcjsPure $ isNull propVal
  newPropVal <- f propVal
  objSetPropertyByName newObj propName $ if isPropNull then pure propVal else toJSVal newPropVal


getNavigatorCredentials :: JSM (Maybe JSVal)
getNavigatorCredentials = do
  nav <- jsg $ s "navigator"
  creds <- nav ^. js (s "credentials")
  cond <- ghcjsPure (isNull creds)
  pure $ if cond
    then Nothing
    else Just creds

jsThen :: (MonadJSM m, MakeObject s) => s -> (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> m JSVal
jsThen promise accept reject = liftJSM $ do
  promise ^. js2 (s "then")
    (fun $ \_ _ [result] -> do
      accept result)
    (fun $ \_ _ [failure] -> do
      reject failure)

postJSONRequest
  :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m)
  => T.Text
  -> Event t T.Text
  -> m (Event t (Either Error T.Text))
postJSONRequest url postDataEv = do
  let
    xhrEv = postDataEv <&> \postData ->
      XhrRequest "POST" url $ def
        & xhrRequestConfig_sendData .~ postData
        & xhrRequestConfig_headers .~ ("Content-type" =: "application/json")
  xhrResponseEv <- performRequestAsyncWithError xhrEv
  pure $ xhrResponseEv <&> \case
    Left xhrException -> Left $ Error_Xhr $ XhrError_Exception xhrException
    Right xhrResponse ->
      case _xhrResponse_responseText xhrResponse of
        Nothing -> Left $ Error_Xhr XhrError_NoResponse
        Just jsonText ->
          let
            errorEither = A.eitherDecodeStrict' $ T.encodeUtf8 jsonText
          in
            case errorEither of
              -- We failed to parse the json as an error, this means we succeeded
              Left _ -> Right jsonText
              -- We successfully parsed the json as an error, this means we got an error!!
              Right err -> Left $ Error_Backend err

jsonParse :: ToJSVal a0 => a0 -> JSM Object
jsonParse jsonText = do
  json <- jsg $ s "JSON"
  json ^. js1 (s "parse") jsonText >>= makeObject

jsonStringify :: ToJSVal a0 => a0 -> JSM T.Text
jsonStringify object = do
  json <- jsg $ s "JSON"
  json ^. js1 (s "stringify") object >>= valToText

data AuthenticatorResponseType
  = Attestation   -- Registration
  | Assertion     -- Login

getPropsByAuthenticatorResponseType :: AuthenticatorResponseType -> [String]
getPropsByAuthenticatorResponseType = \case
  Attestation -> ["attestationObject", "clientDataJSON"]
  Assertion -> ["authenticatorData", "clientDataJSON", "signature", "userHandle"]

encodeBase64PublicKeyCredential :: Object -> AuthenticatorResponseType -> JSM Object
encodeBase64PublicKeyCredential pkCredsObj authRespType = do
  newObj <- create

  -- Copy over properties as is.
  forM_ ["id", "type"] $ \prop ->
    copyProperty pkCredsObj newObj prop

  -- Copy property after encoding it to Base 64 url.
  copyPropertyWithModification toBase64UrlString pkCredsObj newObj "rawId"

  responseObj <- objGetPropertyByName pkCredsObj (s "response") >>= makeObject
  encodeBase64AuthenticatorResponse responseObj authRespType >>=
    objSetPropertyByName newObj (s "response")

  pkCredsObj ^. js0 (s "getClientExtensionResults") >>=
    objSetPropertyByName newObj (s "clientExtensionResults")

  pure newObj

encodeBase64AuthenticatorResponse :: Object -> AuthenticatorResponseType -> JSM Object
encodeBase64AuthenticatorResponse responseObj authRespType = do
  newResponseObj <- create
  forM_ (getPropsByAuthenticatorResponseType authRespType) $ \prop ->
    copyPropertyWithModification toBase64UrlString responseObj newResponseObj prop
  pure newResponseObj

encodeToText :: (A.ToJSON a) => a -> T.Text
encodeToText = T.decodeUtf8 . B.toStrict . A.encode

wrapObjectPublicKey :: Object -> JSM Object
wrapObjectPublicKey objectToWrap = do
  wrapperObj <- create
  objSetPropertyByName wrapperObj (s "publicKey") objectToWrap
  pure wrapperObj

decodeBase64Options :: AuthenticatorResponseType -> Object -> JSM Object
decodeBase64Options authRespType credentialOptionsObj = do
  decodeBase64Property credentialOptionsObj "challenge"

  case authRespType of
    Attestation -> decodeBase64RegistrationOptions credentialOptionsObj
    Assertion -> decodeBase64LoginOptions credentialOptionsObj

  wrapObjectPublicKey credentialOptionsObj

decodeBase64RegistrationOptions :: Object -> JSM ()
decodeBase64RegistrationOptions credentialOptionsObj = do
  userObj <- objGetPropertyByName credentialOptionsObj (s "user") >>= makeObject
  decodeBase64Property userObj "id"
  objSetPropertyByName credentialOptionsObj (s "user") userObj

decodeBase64LoginOptions :: Object -> JSM ()
decodeBase64LoginOptions credentialOptionsObj = do
  (allowCreds :: [JSVal]) <- objGetPropertyByName credentialOptionsObj (s "allowCredentials") >>= fromJSValUncheckedListOf
  forM_ allowCreds $ \allowCred -> do
    allowCredObj <- makeObject allowCred
    decodeBase64Property allowCredObj "id"

  objSetPropertyByName credentialOptionsObj (s "allowCredentials") allowCreds

getMethod :: AuthenticatorResponseType -> String
getMethod = \case
  Attestation -> "create"
  Assertion -> "get"

chainEitherEvents :: (Monad m, Reflex t) => Event t (Either a b) -> (Event t b -> m (Event t (Either a c))) -> m (Event t (Either a c))
chainEitherEvents event f = do
  let
    (err, res) = fanEither event
  event' <- f res
  pure $ leftmost [Left <$> err, event']

setupWorkflow
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => T.Text
  -> AuthenticatorResponseType
  -> Event t T.Text
  -> m (Event t (Either Error T.Text))
setupWorkflow baseUrl authRespType usernameEv = do
  credentialOptionsEv <- postJSONRequest (baseUrl <> "/begin") $ encodeToText . LoginData <$> usernameEv

  promiseResolverEv <-
    credentialOptionsEv `chainEitherEvents` \workflowBeginResultEv ->
      performEventAsync $ processCredentialOptions <$> workflowBeginResultEv


  promiseResolverEv `chainEitherEvents` (postJSONRequest $ baseUrl <> "/complete")
  where
    processCredentialOptions jsonText sendFn = liftJSM $ do
      let
        jsonErrorHandler :: (MonadJSM m) => JSException -> m ()
        jsonErrorHandler = const $ liftIO $ sendFn $ Left $ Error_Frontend FrontendError_JsonParseSyntaxError
      flip catch jsonErrorHandler $ do
        credentialOptionsObj <- jsonParse jsonText

        wrapperObj <- decodeBase64Options authRespType credentialOptionsObj

        getNavigatorCredentials >>= \case
          Nothing -> liftIO $ sendFn $ Left $ Error_Frontend FrontendError_BrowserNotSupported
          Just navCreds -> do
            promise <- navCreds ^. js1 (getMethod authRespType) wrapperObj
            void $ processCredentialOptionsPromise sendFn promise

    processCredentialOptionsPromise sendFn promise =
      jsThen promise
        (\pkCreds -> do
          -- In some cases, this promise may be resolved to null.
          wereCredsNull <- ghcjsPure (isNull pkCreds)
          if wereCredsNull
            then liftIO $ sendFn $ Left $ Error_Frontend FrontendError_NullCredentials
            else do
              pkCredsObj <- makeObject pkCreds

              encodedPkCreds <- encodeBase64PublicKeyCredential pkCredsObj authRespType

              str <- jsonStringify encodedPkCreds

              liftIO $ sendFn $ Right str)
        (\err -> do
          errStr <- valToText err
          let
            failure = case authRespType of
              Attestation -> FrontendError_CreatePromiseRejected errStr
              Assertion -> FrontendError_GetPromiseRejected errStr
          liftIO $ sendFn $ Left $ Error_Frontend failure)

setupRegisterWorkflow
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => T.Text
  -> Event t T.Text
  -> m (Event t (Either Error T.Text))
setupRegisterWorkflow baseUrl usernameEv = setupWorkflow (baseUrl <> "/register") Attestation usernameEv

setupLoginWorkflow
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => T.Text
  -> Event t T.Text
  -> m (Event t (Either Error T.Text))
setupLoginWorkflow baseUrl usernameEv = setupWorkflow (baseUrl <> "/login") Assertion usernameEv
