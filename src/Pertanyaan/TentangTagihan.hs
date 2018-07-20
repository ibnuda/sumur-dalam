{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Pertanyaan.TentangTagihan where

import           Protolude          hiding (from, (<&>))

import           Database.Esqueleto

import           Model

import           Pertanyaan.Bantuan

insertTagihan
  :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Minum
  -> Key Tarif
  -> ReaderT backend m (Key Tagihan)
insertTagihan mid tid = insert $ Tagihan mid tid Nothing
