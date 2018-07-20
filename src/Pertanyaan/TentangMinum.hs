{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangMinum where

import           Protolude          hiding (from, (<&>), on)

import           Database.Esqueleto

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
  -> ReaderT backend m [Entity Meteran]
selectMinumByNomorMeteran nometeran mtahun mbulan = do
  select $ from $ \(meteran `InnerJoin` minum) -> do
    on $ minum ^. MinumMeteranId ==. meteran ^. MeteranId
    where_ $ meteran ^. MeteranNomor ==. val nometeran
    whereOpsional_ minum MinumTahun (fromInteger <$> mtahun)
    whereOpsional_ minum MinumBulan mbulan
    return meteran

-- | Memasukkan data ke database berdasarkan parameter.
insertMinum ::
    ( BaseBackend backend ~ SqlBackend
    , PersistUniqueWrite backend
    , BackendCompatible SqlBackend backend
    , MonadIO m
    )
  => Key Meteran -- ^ Primary key meteran.
  -> Text -- ^ Nomor telepon petugas.
  -> Int64 -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> Int64 -- ^ Penggunaan air pada saat itu.
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
  -> Int64 -- ^ Penggunaan air saat itu.
  -> Maybe (Key Pengguna) -- ^ Petugas pencatat, opsional.
  -> ReaderT SqlBackend m ()
updateMinum meteranid tahun bulan sampai mnomorpetugas = do
  update $ \minum -> do
    set minum [ MinumSampai =. val sampai
              , updateHarusIsi minum MinumPetugasId mnomorpetugas
              ]
    where_ $ minum ^. MinumMeteranId ==. val meteranid
    where_ $ minum ^. MinumTahun ==. val (fromInteger tahun)
    where_ $ minum ^. MinumBulan ==. val bulan
