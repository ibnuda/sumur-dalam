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
import           Bisnis.ValidasiData

import           Pertanyaan.TentangPengguna

-- | Lihat daftar pengguna, meteran, dan apakah penggunaan
--   air pada tahun dan bulan saat ini sudah dicatat atau belum.
lihatPenggunaDanMeteran
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Petugas.
  -> m [(Pengguna, Meteran, Bool)]
lihatPenggunaDanMeteran petugas = do
  Pengguna {..}     <- kewenanganMinimalPengguna petugas Petugas
  (tahun, bulan, _) <- toGregorian . utctDay <$> liftIO getCurrentTime
  pms               <- runDb $ selectPenggunaMeteranDanCatatMinum tahun bulan
  return $ map (\(p, m, s) -> (entityVal p, entityVal m, unValue s)) pms

-- | Menambah pengguna baru.
--   Menerima parameter sesuai dengan `RequestPenggunaBaru`.
--   Alasan menggunakan password dikarenakan mungkin akan
--   ada kebutuhan aplikas pengguna.
tambahPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Admin.
  -> Text -- ^ Nama pengguna baru.
  -> Text -- ^ Nomor telepon pengguna baru.
  -> Text -- ^ Password.
  -> Text -- ^ Alamat pengguna.
  -> Text -- ^ Wilayah pengguna.
  -> Text -- ^ Nomor meteran.
  -> m (Entity Pengguna, Entity Meteran)
tambahPengguna admin nama telp pw alamat wilayah nometeran = do
  _ <- kewenanganMinimalPengguna admin Admin
  _ <- penggunaNil telp
  _ <- meteranHarusNil nometeran
  h <- utctDay <$> liftIO getCurrentTime
  x <- runDb $ do
    Entity pid _   <- insertPengguna nama telp pw Pelanggan alamat wilayah
    Entity _   met <- insertMeteran pid nometeran h
    selectPenggunaMeteran $ meteranNomor met
  case x of
    []  -> throwError $ GagalDB "meteran tidak ada" "saat masukkan pengguna"
    y:_ -> return y
