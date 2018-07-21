{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangSistem where

import           Protolude          hiding (from)

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
