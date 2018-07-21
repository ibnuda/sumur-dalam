{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangPengguna where

import           Protolude          hiding (from, on)

import           Database.Esqueleto

import           Model
import           Model.Grouping
import           Util

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
    let something mid = case_
          [ when_
              ( exists $ from $ \m -> do
                where_ $ m ^. MinumTahun ==. val (fromInteger tahun)
                where_ $ m ^. MinumBulan ==. val bulan
                where_ $ m ^. MinumMeteranId ==. mid
              )
              then_
              (val $ True)
          ]
          (else_ $ val False)
    orderBy [desc (pengguna ^. PenggunaAlamat)]
    return (pengguna, meteran, something (meteran ^. MeteranId))
