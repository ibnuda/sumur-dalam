{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangSistem where

import           Protolude          hiding (from, isNothing, on)

import           Database.Esqueleto

import           Model
import           Model.Grouping

selectTarifTerbaru
  :: ( PersistUniqueRead b
     , PersistQueryRead b
     , BackendCompatible SqlBackend b
     , MonadIO m
     )
  => ReaderT b m [Entity Tarif]
selectTarifTerbaru = do
  select $ from $ \tarif -> do
    orderBy [desc (tarif ^. TarifId)]
    limit 1
    return tarif

insertGrup
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => GrupSistem -- ^ Grup
  -> ReaderT backend m (Key Grup)
insertGrup g = insert $ Grup g

insertTarif
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Int64
  -> Int64
  -> Int64
  -> Int64
  -> Int64
  -> Int64
  -> ReaderT backend m (Key Tarif)
insertTarif a b c d e f = insert $ Tarif a b c d e f

selectJumlahPelanggan
  :: ( PersistUniqueRead b
     , PersistQueryRead b
     , BackendCompatible SqlBackend b
     , MonadIO m
     )
  => ReaderT b m [Value Int64]
selectJumlahPelanggan = do
  select $ from $ \(grup `InnerJoin` pengguna `InnerJoin` meteran) -> do
    on $ meteran ^. MeteranPenggunaId ==. pengguna ^. PenggunaId
    on $ pengguna ^. PenggunaGrupId ==. grup ^. GrupId
    where_ $ grup ^. GrupNama ==. val Pelanggan
    where_ $ isNothing $ meteran ^. MeteranTanggalPutus
    return (count (meteran ^. MeteranId))

selectTagihanLunas
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Int64
  -> Int
  -> Bool
  -> ReaderT backend m [Value Int64]
selectTagihanLunas tahun bulan sudahbayar = do
  select $ from $ \(minum `InnerJoin` tagihan) -> do
    on $ tagihan ^. TagihanMinumId ==. minum ^. MinumId
    where_ $ minum ^. MinumTahun ==. val tahun
    where_ $ minum ^. MinumBulan ==. val bulan
    sudahdibayar tagihan sudahbayar
    return (count (tagihan ^. TagihanId))
 where
  sudahdibayar tagihan True =
    where_ $ not_ $ isNothing $ tagihan ^. TagihanTanggalBayar
  sudahdibayar tagihan False =
    where_ $ isNothing $ tagihan ^. TagihanTanggalBayar
