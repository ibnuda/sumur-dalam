{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangTagihan where

import           Protolude          hiding (from, (<&>), on)

import           Database.Esqueleto

import           Model

import           Pertanyaan.Bantuan

insertTagihan
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Minum
  -> Key Tarif
  -> ReaderT backend m (Key Tagihan)
insertTagihan mid tid = insert $ Tagihan mid tid Nothing

selectDaftarTagihanByTahunBulan
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Int64
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
selectDaftarTagihanByTahunBulan tini bini tlalu blalu = do
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
          let minumbulanlalu =
                minumDiTahunBulan (meteran ^. MeteranId) (val tlalu) (val blalu)
          return
            ( pengguna
            , meteran
            , tagihan
            , minum
            , tarif
            , minumbulanlalu
            )
