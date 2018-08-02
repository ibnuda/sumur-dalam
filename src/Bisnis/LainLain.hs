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
import           Bisnis.ValidasiData

import           Pertanyaan.TentangSistem
import           Pertanyaan.TentangMinum

lihatTarifTerbaru
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m Tarif
lihatTarifTerbaru p = do
  _ <- kewenanganMinimalPengguna p Pelanggan
  t <- runDb $ selectTarif $ Just 1
  case t of
    []    -> throwError GagalTarifKosong
    x : _ -> return $ entityVal x

lihatDaftarTarif
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m [Tarif]
lihatDaftarTarif p = do
  _ <- kewenanganMinimalPengguna p Pelanggan
  t <- runDb $ selectTarif Nothing
  case t of
    [] -> throwError GagalTarifKosong
    x  -> return $ map entityVal x

tambahTarif
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> Int64
  -> Int64
  -> Int64
  -> Int64
  -> Int64
  -> Int64
  -> m (Entity Tarif)
tambahTarif admin a b c d e f = do
  _ <- kewenanganMinimalPengguna admin Admin
  runDb $ insertTarif a b c d e f

lihatIkhtisar
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m (Int64, Int64, Int64, Tarif)
lihatIkhtisar a = do
  _              <- kewenanganMinimalPengguna a Admin
  (tahun, bulan) <- tahunBulanHarusValid Nothing Nothing
  res            <- runDb $ do
    p <- selectJumlahPelanggan
    l <- selectTagihanLunas (fromInteger tahun) bulan True
    b <- selectTagihanLunas (fromInteger tahun) bulan False
    t <- selectTarif $ Just 1
    return (p, l, b, t)
  case res of
    ([Value pelanggan], [Value lunas], [Value belum], [tarif]) ->
      return (pelanggan, lunas, belum, entityVal tarif)
    _ -> throwError $ GagalDB "Lihat Ikhtisar" "Data tidak lengkap."

lihatRiwayatMinum
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m [(Value Int, Value Int, Value (Maybe Rational))]
lihatRiwayatMinum a = do
  _ <- kewenanganMinimalPengguna a Admin
  runDb selectSemuaMinum
