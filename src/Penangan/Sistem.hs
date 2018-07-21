{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Penangan.Sistem where

import           Protolude

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.LainLain

getTarifTerbaruPenangan
  :: MonadIO m => AuthResult Pengguna -> PenanganT m ResponseDataTagihanTarif
getTarifTerbaruPenangan (Authenticated x) = do
  tarifKeResponse <$> lihatTarifTerbaru x
getTarifTerbaruPenangan _ = throwError $ GagalTakBerwenang "Tidak boleh lihat."
