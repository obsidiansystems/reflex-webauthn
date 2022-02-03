{-# Language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TypeFamilies #-}

module Reflex.WebAuthn.DB.DB where

import Data.Bits as Bits
import Data.Maybe (isJust)
import Data.Pool
import qualified Data.Text as T
import Data.Word
import qualified Crypto.WebAuthn as WA

import Database.Beam
import qualified Database.Beam.AutoMigrate as BA
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple.Transaction as PT

import Reflex.WebAuthn.DB.User as U
import Reflex.WebAuthn.DB.CredentialEntry as C

data UserDb f = UserDb
  { users :: f (TableEntity UserT)
  , credentialEntries :: f (TableEntity CredentialEntryT)
  } deriving (Generic, Database be)

userDb :: DatabaseSettings be UserDb
userDb = defaultDbSettings `withDbModification` dbModification
  { users = modifyTableFields tableModification
      { U.accountName = fieldNamed "account_name"
      , U.accountDisplayName = fieldNamed "account_display_name"
      }
  , credentialEntries = modifyTableFields tableModification
      { C.credentialId = fieldNamed "credential_id"
      , C.userHandle = UserId $ fieldNamed "user_handle"
      , C.publicKey = fieldNamed "public_key"
      , C.signCounter = fieldNamed "sign_counter"
      }
  }

userDbPostgres :: BA.AnnotatedDatabaseSettings Postgres UserDb
userDbPostgres = BA.defaultAnnotatedDbSettings userDb

initDb :: Pool Connection -> IO ()
initDb pool = do
  withResource pool $ \dbConn ->
    BA.tryRunMigrationsWithEditUpdate userDbPostgres dbConn

withTransaction :: Pool Connection -> Pg a -> IO a
withTransaction pool sql = withResource pool $ \conn ->
  PT.withTransaction conn $ runBeamPostgres conn sql

insertUser :: Pool Connection -> WA.CredentialUserEntity -> IO ()
insertUser pool WA.CredentialUserEntity {..} = withTransaction pool $
  runInsert $
    insert (users userDb) $ insertValues [user]
  where
    user = UserT
      { handle = WA.unUserHandle cueId
      , accountName = WA.unUserAccountDisplayName cueDisplayName
      , accountDisplayName = WA.unUserAccountName cueName
      }

insertCredentialEntry :: Pool Connection -> WA.CredentialEntry -> IO ()
insertCredentialEntry pool WA.CredentialEntry {..} = withTransaction pool $
  runInsert $
    insert (credentialEntries userDb) $ insertValues [cred]
  where
    cred = C.CredentialEntryT
      { credentialId = WA.unCredentialId ceCredentialId
      , userHandle = UserId $ WA.unUserHandle ceUserHandle
      , publicKey = WA.unPublicKeyBytes cePublicKeyBytes
      , signCounter = WA.unSignatureCounter ceSignCounter
      , transports = transportsToBits ceTransports
      }

checkIfUserExists :: Pool Connection -> T.Text -> IO Bool
checkIfUserExists pool username = fmap isJust $ withTransaction pool $
  runSelectReturningOne $ select $ do
    user <- all_ (users userDb)
    guard_ (U.accountName user ==. val_ username)
    pure user

getCredentialEntryByUser :: Pool Connection -> T.Text -> IO [WA.CredentialEntry]
getCredentialEntryByUser pool username = fmap (map toCredentialEntry) $ withTransaction pool $
  runSelectReturningList $ select $ do
    user <- all_ (users userDb)
    credentialEntry <- all_ (credentialEntries userDb)
    guard_ (UserId (U.handle user) ==. C.userHandle credentialEntry &&. U.accountName user ==. val_ username)
    pure credentialEntry

getCredentialEntryByCredentialId :: Pool Connection -> WA.CredentialId -> IO (Maybe WA.CredentialEntry)
getCredentialEntryByCredentialId pool (WA.CredentialId credId) = fmap (fmap toCredentialEntry) $ withTransaction pool $
  runSelectReturningOne $ select $ do
    credentialEntry <- all_ (credentialEntries userDb)
    guard_ (C.credentialId credentialEntry ==. val_ credId)
    pure credentialEntry

updateSignatureCounter :: Pool Connection -> WA.CredentialId -> WA.SignatureCounter -> IO ()
updateSignatureCounter pool (WA.CredentialId credId) (WA.SignatureCounter counter) = withTransaction pool $
  runUpdate $
    update (credentialEntries userDb)
      (\credEntry -> signCounter credEntry <-. val_ counter)
      (\credEntry -> credentialId credEntry ==. val_ credId)

toCredentialEntry :: C.CredentialEntry -> WA.CredentialEntry
toCredentialEntry ce = WA.CredentialEntry
  { WA.ceCredentialId = WA.CredentialId $ C.credentialId ce
  , WA.ceUserHandle = WA.UserHandle $ unUserId $ C.userHandle ce
  , WA.cePublicKeyBytes = WA.PublicKeyBytes $ C.publicKey ce
  , WA.ceSignCounter = WA.SignatureCounter $ C.signCounter ce
  , WA.ceTransports = transportsFromBits $ C.transports ce
  }
  where
    unUserId (UserId f) = f

transportsToBits :: [WA.AuthenticatorTransport] -> Word32
transportsToBits [] = Bits.zeroBits
transportsToBits (WA.AuthenticatorTransportInternal : xs) = transportsToBits xs `Bits.setBit` 0
transportsToBits (WA.AuthenticatorTransportUSB : xs) = transportsToBits xs `Bits.setBit` 1
transportsToBits (WA.AuthenticatorTransportBLE : xs) = transportsToBits xs `Bits.setBit` 2
transportsToBits (WA.AuthenticatorTransportNFC : xs) = transportsToBits xs `Bits.setBit` 3

transportsFromBits :: Word32 -> [WA.AuthenticatorTransport]
transportsFromBits bits =
  [WA.AuthenticatorTransportInternal | Bits.testBit bits 0]
    ++ [WA.AuthenticatorTransportUSB | Bits.testBit bits 1]
    ++ [WA.AuthenticatorTransportBLE | Bits.testBit bits 2]
    ++ [WA.AuthenticatorTransportNFC | Bits.testBit bits 3]
