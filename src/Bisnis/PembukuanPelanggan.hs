{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.PembukuanPelanggan
  ( lihatPenggunaDanMeteran
  , tambahPengguna
  ) where

import           Protolude

import           Database.Esqueleto
import           Data.Time

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas

import           Pertanyaan.TentangPengguna

lihatPenggunaDanMeteran
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m [(Pengguna, Meteran, Bool)]
lihatPenggunaDanMeteran petugas = do
  Pengguna {..}     <- kewenanganMinimalPengguna petugas Petugas
  (tahun, bulan, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
  pms               <- runDb $ selectPenggunaMeteranDanCatatMinum tahun bulan
  return $ map (\(p, m, s) -> (entityVal p, entityVal m, unValue s)) pms

tambahPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> m (Entity Pengguna)
tambahPengguna admin nama telp pw alamat wilayah = do
  _ <- kewenanganMinimalPengguna admin Admin
  runDb $ insertPengguna nama telp pw Pelanggan alamat wilayah

