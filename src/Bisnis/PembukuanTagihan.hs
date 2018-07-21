{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.PembukuanTagihan where

import           Protolude

import           Database.Esqueleto
import           Data.Time

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas
import           Bisnis.ValidasiData

import Pertanyaan.TentangTagihan

lihatDaftarTagihan
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Maybe Integer
  -> Maybe Int
  -> m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         , Value Int64
         )
       ]
lihatDaftarTagihan admin mtahun mbulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  (tini , bini ) <- tahunBulanHarusValid mtahun mbulan
  (tlalu, blalu) <- tahunBulanLalu tini bini
  runDb $ selectDaftarTagihanByTahunBulan (fromInteger tini)
                                          bini
                                          (fromInteger tlalu)
                                          blalu
