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

bayarTagihan admin nomortagihan = do
  _ <- kewenanganMinimalPengguna admin Admin
  _ <- tagihanAda nomortagihan
  h <- utctDay <$> liftIO getCurrentTime
  _ <- runDb $ updateTagihan (toSqlKey nomortagihan) h
  return ()

lihatTagihanPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> Maybe Integer
  -> Maybe Int
  -> m
       ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       , Value Int64
       )
lihatTagihanPengguna admin nomortelepon mtahun mbulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  _              <- penggunaPunyaMeteran nomortelepon
  (tini , bini ) <- tahunBulanHarusValid mtahun mbulan
  (tlalu, blalu) <- tahunBulanLalu tini bini
  tagihan        <- runDb $ selectDaftarTagihanByTahunBulan Nothing
                                                            (fromInteger tini)
                                                            bini
                                                            (fromInteger tlalu)
                                                            blalu
  case tagihan of
    []  -> throwError $ GagalTagihanTahunBulanNil tini bini
    x:_ -> return x

lihatDaftarTagihanPengguna admin nomortelepon = do
  _ <- kewenanganMinimalPengguna admin Admin
  _ <- penggunaPunyaMeteran nomortelepon
  return ()
