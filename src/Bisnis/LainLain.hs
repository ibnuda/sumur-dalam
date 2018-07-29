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

lihatTarifTerbaru
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna
  -> m Tarif
lihatTarifTerbaru p = do
  _ <- kewenanganMinimalPengguna p Pelanggan
  t <- runDb selectTarifTerbaru
  case t of
    []    -> throwError GagalTarifKosong
    x : _ -> return $ entityVal x

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
    t <- selectTarifTerbaru
    return (p, l, b, t)
  case res of
    ([Value pelanggan], [Value lunas], [Value belum], [tarif]) ->
      return (pelanggan, lunas, belum, entityVal tarif)
    _ -> throwError $ GagalDB "Lihat Ikhtisar" "Data tidak lengkap."
