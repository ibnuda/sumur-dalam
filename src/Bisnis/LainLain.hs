{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.LainLain where

import           Protolude

import           Database.Esqueleto

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas

import           Pertanyaan.TentangSistem

lihatTarifTerbaru
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m Tarif
lihatTarifTerbaru p = do
  _ <- kewenanganMinimalPengguna p Pelanggan
  t <- runDb selectTarifTerbaru
  case t of
    []  -> throwError GagalTarifKosong
    x:_ -> return $ entityVal x
