{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangMinum where

import           Protolude          hiding (from, (<&>), on, isNothing)

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

import           Model

import           Pertanyaan.Bantuan

-- | Melihat minum yang tercatat berdasarkan parameter.
selectMinumByNomorMeteran
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text -- ^ Nomor meteran.
  -> Maybe Integer -- ^ Tahun pencatatan, opsional.
  -> Maybe Int -- ^ Bulan pencatatan, opsional.
  -> ReaderT backend m [(Entity Meteran, Entity Minum)]
selectMinumByNomorMeteran nometeran mtahun mbulan = do
  select $ from $ \(meteran `InnerJoin` minum) -> do
    on $ minum ^. MinumMeteranId ==. meteran ^. MeteranId
    where_ $ meteran ^. MeteranNomor ==. val nometeran
    whereOpsional_ minum MinumTahun (fromInteger <$> mtahun)
    whereOpsional_ minum MinumBulan mbulan
    return (meteran, minum)

-- | Lihat daftar penggunaan air oleh semua orang.
selectSemuaMinum
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [(Value Int, Value Int, Value (Maybe Rational))]
selectSemuaMinum = do
  select $ from $ \minum -> do
    groupBy $ minum ^. MinumTahun
    groupBy $ minum ^. MinumBulan
    orderBy [ desc (minum ^. MinumTahun), desc (minum ^. MinumBulan) ]
    return (minum ^. MinumTahun, minum ^. MinumBulan, sum_ (minum ^. MinumSampai))

-- | Memasukkan data ke database berdasarkan parameter.
insertMinum
  :: ( BaseBackend backend ~ SqlBackend
     , PersistUniqueWrite backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Meteran -- ^ Primary key meteran.
  -> Text -- ^ Nomor telepon petugas.
  -> Int -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> Int -- ^ Penggunaan air pada saat itu.
  -> ReaderT backend m (Entity Minum)
insertMinum meteranid nomorpetugas tahun bulan sampai = do
  Just (Entity pid _) <- getBy $ UniqueNomorTelp nomorpetugas
  insertEntity $ Minum meteranid pid tahun bulan sampai

-- | Mengubah data di database berdasarkan parameter.
updateMinum
  :: MonadIO m
  => Key Meteran -- ^ Primary key meteran.
  -> Integer -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> Int -- ^ Penggunaan air saat itu.
  -> Maybe (Key Pengguna) -- ^ Petugas pencatat, opsional.
  -> ReaderT SqlBackend m ()
updateMinum meteranid tahun bulan sampai mnomorpetugas = do
  update $ \minum -> do
    set
      minum
      [ MinumSampai =. val sampai
      , updateHarusIsi minum MinumPetugasId mnomorpetugas
      ]
    where_ $ minum ^. MinumMeteranId ==. val meteranid
    where_ $ minum ^. MinumTahun ==. val (fromInteger tahun)
    where_ $ minum ^. MinumBulan ==. val bulan

-- | Lihat meteran berdasarkan nomor meteran dan belum ditutup.
selectMeteran
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text -- ^ Nomor meteran.
  -> ReaderT backend m [Entity Meteran]
selectMeteran nometer = do
  select $ from $ \meteran -> do
    where_ $ meteran ^. MeteranNomor ==. val nometer
    where_ $ isNothing $ meteran ^. MeteranTanggalPutus
    return meteran

-- | Lihat meteran berdasarkan nomor meteran, terlepas sudah
--   ditutup atau belum.
selectMeteran'
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text -- ^ Nomor meteran.
  -> ReaderT backend m [Entity Meteran]
selectMeteran' nometer = do
  select $ from $ \meteran -> do
    where_ $ meteran ^. MeteranNomor ==. val nometer
    return meteran
