{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conf where

import           Protolude

import           Control.Monad.Trans.Reader  hiding (asks)

import           Database.Persist.Postgresql
import           Servant
import           Servant.Auth.Server

import           Model
import           Types

data Konfigurasi = Konfigurasi
  { konfigurasiPool        :: ConnectionPool
  , konfigurasiSettingJWT  :: JWTSettings
  , konfigurasiGrupYangAda :: [Entity Grup]
  }

newtype PenanganT m a = PenanganT
  { runPenangan :: ReaderT Konfigurasi (ExceptT Gagal m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Konfigurasi
             , MonadError Gagal
             , MonadIO
             )

type Penangan = PenanganT IO

penanganKeHandler :: Konfigurasi -> Penangan a -> Handler a
penanganKeHandler conf coach = do
  let readerconf = runPenangan coach
      servanterr = mapReaderT (withExceptT gagalToServantErr) readerconf
  Handler (runReaderT servanterr conf)

data SetingDB = SetingDB
  { settingdbName     :: Text
  , settingdbUsername :: Text
  , settingdbPassword :: Text
  , settingdbPort     :: Int
  , settingdbHost     :: Text
  }

runDb :: (MonadIO m, MonadReader Konfigurasi m) => SqlPersistT IO b -> m b
runDb q = do
  pool <- asks konfigurasiPool
  liftIO $ runSqlPool q pool
