{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangTagihan where

import           Protolude          hiding (from, on, (<&>))

import           Database.Esqueleto

import           Data.Time

import           Model

import           Pertanyaan.Bantuan

insertTagihan
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Minum
  -> Key Tarif
  -> ReaderT backend m (Key Tagihan)
insertTagihan mid tid = insert $ Tagihan mid tid Nothing

selectTagihan
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Tagihan
  -> ReaderT backend m [Entity Tagihan]
selectTagihan tid = do
  select $ from $ \tagihan -> do
    where_ $ tagihan ^. TagihanId ==. val tid
    limit 1
    return tagihan

updateTagihan :: MonadIO m => Key Tagihan -> Day -> ReaderT SqlBackend m ()
updateTagihan tid h = do
  update $ \tagihan -> do
    set tagihan [TagihanTanggalBayar =. (just $ val h)]
    where_ $ tagihan ^. TagihanId ==. val tid

selectDaftarTagihanByTahunBulan
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> Int64
  -> Int
  -> Int64
  -> Int
  -> ReaderT
       backend
       m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         , Value Int64
         )
       ]
selectDaftarTagihanByTahunBulan mnotelp tini bini tlalu blalu = do
  select
    $ from
    $ \(pengguna `InnerJoin` meteran `LeftOuterJoin` minum `LeftOuterJoin` tagihan `InnerJoin` tarif) ->
        do
          on $ tagihan ^. TagihanTarifId ==. tarif ^. TarifId
          on $ minum ^. MinumId ==. tagihan ^. TagihanMinumId
          on $ meteran ^. MeteranId ==. minum ^. MinumMeteranId
          on $ pengguna ^. PenggunaId ==. meteran ^. MeteranPenggunaId
          where_ $ minum ^. MinumTahun ==. val tini
          where_ $ minum ^. MinumBulan ==. val bini
          whereOpsional_ pengguna PenggunaNomorTelp mnotelp
          let minumbulanlalu = minumDiTahunBulan (meteran ^. MeteranId)
                                                 (val tlalu)
                                                 (val blalu)
          return (pengguna, meteran, tagihan, minum, tarif, minumbulanlalu)

selectTagihanPengguna
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> ReaderT
       backend
       m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         )
       ]
selectTagihanPengguna notelp = do
  select
    $ from
    $ \(pengguna `InnerJoin` meteran `LeftOuterJoin` minum `LeftOuterJoin` tagihan `InnerJoin` tarif) ->
        do
          on $ tagihan ^. TagihanTarifId ==. tarif ^. TarifId
          on $ minum ^. MinumId ==. tagihan ^. TagihanMinumId
          on $ meteran ^. MeteranId ==. minum ^. MinumMeteranId
          on $ pengguna ^. PenggunaId ==. meteran ^. MeteranPenggunaId
          where_ $ pengguna ^. PenggunaNomorTelp ==. val notelp
          orderBy [asc (minum ^. MinumId)]
          return (pengguna, meteran, tagihan, minum, tarif)
