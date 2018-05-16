{-# LANGUAGE OverloadedStrings #-}

module Network.VaultTool.Trans.Database.Postgresql
    ( connect
    ) where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Text                        (unpack)
import qualified Database.PostgreSQL.Simple       as PSQL
import           GHC.Word                         (Word16)

import           Network.VaultTool.Trans          (VaultT (..))
import           Network.VaultTool.Trans.Database (readCredentials)


connect :: MonadIO m => String -> Word16 -> String -> VaultT m PSQL.Connection
connect host port database = do
    (username, password) <- readCredentials
    liftIO . PSQL.connect $ PSQL.ConnectInfo host port (unpack username) (unpack password) database
