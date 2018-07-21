{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.PembukuanTagihan where

import           Protolude

import           Data.Time
import           Database.Esqueleto

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas
import           Bisnis.ValidasiData

import           Pertanyaan.TentangTagihan

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
  runDb $ selectDaftarTagihanByTahunBulan Nothing
                                          (fromInteger tini)
                                          bini
                                          (fromInteger tlalu)
                                          blalu

bayarTagihan
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> Integer
  -> Int
  -> m
       ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       , Value Int64
       )
bayarTagihan admin notelp tahun bulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  (tini , bini ) <- tahunBulanHarusValid (Just tahun) (Just bulan)
  (tlalu, blalu) <- tahunBulanLalu tini bini
  hariini        <- utctDay <$> liftIO getCurrentTime
  Entity tid _   <- tagihanAda notelp (fromInteger tini) bini
  tagihan        <- runDb $ do
    updateTagihan tid hariini
    selectDaftarTagihanByTahunBulan (Just notelp)
                                    (fromInteger tini)
                                    bini
                                    (fromInteger tlalu)
                                    blalu
  case tagihan of
    []  -> throwError $ GagalTagihanTahunBulanNil tini bini
    x:_ -> return x

lihatTagihanPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> Integer
  -> Int
  -> m
       ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       , Value Int64
       )
lihatTagihanPengguna admin nomortelepon tahun bulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  _              <- penggunaPunyaMeteran nomortelepon
  (tini , bini ) <- tahunBulanHarusValid (Just tahun) (Just bulan)
  (tlalu, blalu) <- tahunBulanLalu tini bini
  tagihan        <- runDb $ selectDaftarTagihanByTahunBulan Nothing
                                                            (fromInteger tini)
                                                            bini
                                                            (fromInteger tlalu)
                                                            blalu
  case tagihan of
    []  -> throwError $ GagalTagihanTahunBulanNil tini bini
    x:_ -> return x

lihatDaftarTagihanPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         )
       ]
lihatDaftarTagihanPengguna admin nomortelepon = do
  _ <- kewenanganMinimalPengguna admin Admin
  _ <- penggunaPunyaMeteran nomortelepon
  runDb $ selectTagihanPengguna Nothing (Just nomortelepon)
