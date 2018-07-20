{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.PembukuanAir where

import           Protolude

import           Database.Esqueleto

import           Data.Time               (getCurrentTime, toGregorian)
import           Data.Time.Clock         (utctDay)

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas
import           Bisnis.ValidasiData

import           Pertanyaan.TentangMinum
import           Pertanyaan.TentangTagihan

-- | Mencatat penggunaan air pada meteran dengan nomor di parameter
--   pada bulan ini.
catatAirBulanIni
  :: (MonadReader Konfigurasi m, MonadError Gagal m, MonadIO m)
  => Pengguna -- ^ Pencatat.
  -> Text -- ^ Nomor meteran.
  -> Int64 -- ^ Sampai.
  -> m (Integer, Int, Int64)
catatAirBulanIni petugas nomormeteran sampai = do
  Pengguna {..}     <- kewenanganMinimalPengguna petugas Petugas
  (tahun, bulan, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
  (Entity mid met)  <- meteranHarusAda nomormeteran
  _                 <- meteranBulanIniHarusKosong (meteranNomor met) tahun bulan
  (Entity tid _)    <- tarifTerbaru
  runDb $ do
    entityminum <- insertMinum mid
                               penggunaNomorTelp
                               (fromInteger tahun)
                               bulan
                               sampai
    _ <- insertTagihan (entityKey entityminum) tid
    return (tahun, bulan, sampai)

-- | Mengubah penggunaan air pada meteran dengan nomor di parameter
--   pada bulan ini.
ubahCatatanAirBulanIni
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Pencatat.
  -> Text -- ^ Nomor meteran.
  -> Int64 -- ^ Sampai.
  -> m (Integer, Int, Int64)
ubahCatatanAirBulanIni petugas nomormeteran sampai = do
  Pengguna {..}     <- kewenanganMinimalPengguna petugas Petugas
  (tahun, bulan, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
  _                 <- meteranHarusAda nomormeteran
  (Entity mid _)    <- meteranBulanIniHarusIsi nomormeteran tahun bulan
  runDb $ updateMinum mid tahun bulan sampai Nothing
  return (tahun, bulan, sampai)

lihatDaftarCatatanAir petugas mtahun mbulan = do
  Pengguna {..} <- kewenanganMinimalPengguna petugas Petugas
  (tahun, bulan) <- tahunBulanHarusValid mtahun mbulan
  return ()
