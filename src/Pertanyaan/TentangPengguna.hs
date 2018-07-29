{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangPengguna where

import           Protolude          hiding (from, on, isNothing)

import           Database.Esqueleto
import           Data.Time

import           Model
import           Model.Grouping
import           Util

import           Pertanyaan.Bantuan

insertPengguna
  :: ( BaseBackend backend ~ SqlBackend
     , PersistStoreWrite backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => Text -- ^ Nama.
  -> Text -- ^ Nomor Telp.
  -> Text -- ^ Password.
  -> GrupSistem -- ^ Grup.
  -> Text -- ^ Alamat.
  -> Text -- ^ Wilayah.
  -> ReaderT backend m (Entity Pengguna)
insertPengguna nama telp password grup alamat wilayah = do
  mgrup <- getBy $ UniqueGrup grup
  case mgrup of
    Nothing -> panic ""
    Just g  -> do
      passw <- liftIO $ buatPassword password
      insertEntity $ Pengguna nama telp passw (entityKey g) alamat wilayah

updatePengguna
  :: MonadIO m
  => Text -- ^ Nomor telepon.
  -> Maybe Text -- ^ Nama, opsional.
  -> Maybe Text -- ^ Nomor telepon, opsional.
  -> Maybe (Key Grup) -- ^ Grup Id, opsional.
  -> Maybe Text -- ^ Alamat, opsional.
  -> Maybe Text -- ^ Wilayah, opsional.
  -> ReaderT SqlBackend m ()
updatePengguna notelp mnama mpassw mgrup malamat mwilayah = do
  update $ \pengguna -> do
    set
      pengguna
      [ updateHarusIsi pengguna PenggunaNama     mnama
      , updateHarusIsi pengguna PenggunaPassword mpassw
      , updateHarusIsi pengguna PenggunaGrupId   mgrup
      , updateHarusIsi pengguna PenggunaAlamat   malamat
      , updateHarusIsi pengguna PenggunaWilayah  mwilayah
      ]
    where_ $ pengguna ^. PenggunaNomorTelp ==. val notelp

selectPenggunaMeteranDanCatatMinum
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Integer
  -> Int
  -> ReaderT backend m [(Entity Pengguna, Entity Meteran, Value Bool)]
selectPenggunaMeteranDanCatatMinum tahun bulan = do
  select $ from $ \(pengguna `InnerJoin` meteran) -> do
    on $ pengguna ^. PenggunaId ==. meteran ^. MeteranPenggunaId
    let sudahcatat mid = case_
          [ when_
              (exists $ from $ \m -> do
                where_ $ m ^. MinumTahun ==. val (fromInteger tahun)
                where_ $ m ^. MinumBulan ==. val bulan
                where_ $ m ^. MinumMeteranId ==. mid
              )
              then_
              (val $ True)
          ]
          (else_ $ val False)
    where_ $ isNothing $ meteran ^. MeteranTanggalPutus
    orderBy [desc (pengguna ^. PenggunaAlamat)]
    return (pengguna, meteran, sudahcatat (meteran ^. MeteranId))

selectPenggunaMeteran
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> ReaderT backend m [(Entity Pengguna, Entity Meteran)]
selectPenggunaMeteran mnometeran = do
  select $ from $ \(pengguna `InnerJoin` meteran) -> do
    on $ pengguna ^. PenggunaId ==. meteran ^. MeteranPenggunaId
    whereOpsional_ meteran MeteranNomor mnometeran
    return (pengguna, meteran)

insertMeteran
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Pengguna
  -> Text
  -> Day
  -> ReaderT backend m (Entity Meteran)
insertMeteran pid nometeran h = do
  insertEntity $ Meteran pid nometeran h Nothing

selectMeteranPengguna
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> ReaderT backend m [Entity Meteran]
selectMeteranPengguna notelp = do
  select $ from $ \(pengguna `InnerJoin` meteran) -> do
    on $ meteran ^. MeteranPenggunaId ==. pengguna ^. PenggunaId
    where_ $ pengguna ^. PenggunaNomorTelp ==. val notelp
    limit 1
    return meteran

selectPenggunaByNomorTelepon
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> ReaderT backend m [Entity Pengguna]
selectPenggunaByNomorTelepon notelp = do
  select $ from $ \pengguna -> do
    where_ $ pengguna ^. PenggunaNomorTelp ==. val notelp
    return pengguna
