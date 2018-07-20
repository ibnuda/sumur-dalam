{-# LANGUAGE FlexibleContexts #-}
module Pertanyaan.TentangSistem where

import           Protolude          hiding (from)

import           Database.Esqueleto

import           Model

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
