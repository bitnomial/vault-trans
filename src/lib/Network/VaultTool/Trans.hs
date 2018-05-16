{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.VaultTool.Trans
    ( VaultT (..)
    , runVaultT

    , -- * Types
      VaultConnectionInfo (..)
    ) where

import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Network.VaultTool          (VaultConnection, VaultSecretPath (..))


newtype VaultT m a = VaultT { unVaultT :: ReaderT VaultConnectionInfo m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)


data VaultConnectionInfo = VaultConnectionInfo
    { conn       :: VaultConnection
    , secretPath :: VaultSecretPath
    }


runVaultT :: MonadIO m => VaultConnection -> VaultSecretPath -> VaultT m a -> m a
runVaultT vc vsp = flip runReaderT (VaultConnectionInfo vc vsp) . unVaultT
