{-# LANGUAGE OverloadedStrings #-}

module Network.VaultTool.Trans.Database
    ( readCredentials
    ) where

import           Control.Monad              (liftM2)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ask)
import           Data.Aeson                 (withText)
import           Data.Aeson.Types           (Object, Value, parseMaybe)
import qualified Data.HashMap.Strict        as M
import           Data.Maybe                 (maybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Network.VaultTool          (VaultConnection, VaultSecretMetadata, VaultSecretPath (..), vaultRead)

import           Network.VaultTool.Trans    (VaultConnectionInfo (conn, secretPath), VaultT (..))

readCredentials :: MonadIO m => VaultT m (Text, Text)
readCredentials = do
    (_, secret) <- VaultT $ ask >>= liftIO . liftM2 vaultRead' conn secretPath
    case secret of
      Left (v,s) -> fail ("Unable to retrieve credentials - " <> show v <> s)
      Right s    -> do
          username <- liftIO $ lookupField "username" s
          password <- liftIO $ lookupField "password" s
          return (username, password)
  where
    vaultRead' :: VaultConnection -> VaultSecretPath -> IO (VaultSecretMetadata, Either (Value, String) Object)
    vaultRead' = vaultRead

    lookupField :: Text -> Object -> IO Text
    lookupField f = maybe (fail $ "Unable to retrieve " <> unpack f) (parseField f) . M.lookup f

    parseField :: Text -> Value -> IO Text
    parseField f = maybe (fail $ "Unable to parse " <> unpack f) return . parseMaybe (withText (unpack f) return)
