{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Penangan.Pintu
  ( postMasukPenangan
  ) where

import           Protolude

import           Servant

import           Database.Esqueleto (fromSqlKey)

import           Conf
import           Model
import           Types

import           Bisnis.Otoritas

postMasukPenangan
  :: MonadIO m => RequestMasuk -> PenanganT m ResponseDataPelangganToken
postMasukPenangan RequestMasuk {..} = do
  (Pengguna {..}, token) <- otentikasi reqmasukNomorTelepon reqmasukPassword
  return $ ResponseDataPelangganToken penggunaNama
                                      penggunaNomorTelp
                                      (fromSqlKey penggunaGrupId)
                                      token
