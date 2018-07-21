{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Pertanyaan.Bantuan where

import           Protolude                            hiding (from)

import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language

import           Model

-- | Bantuan untuk query atas field dengan parameter opsional.
whereOpsional_
  :: (PersistField typ, Esqueleto query expr backend, PersistEntity val)
  => expr (Entity val) -- ^ Tabel.
  -> EntityField val typ -- ^ Kolom di tabel pertama.
  -> Maybe typ -- ^ Parameter opsional.
  -> query ()
whereOpsional_ ent acc (Just x) = where_ $ ent ^. acc ==. val x
whereOpsional_ _   _   _        = return ()

updateHarusIsi
  :: (PersistField typ, Esqueleto query expr backend, PersistEntity val)
  => expr (Entity val) -- ^ Tabel.
  -> EntityField val typ -- ^ Kolom di tabel.
  -> Maybe typ -- ^ Parameter opsional.
  -> expr (Update val)
updateHarusIsi ent acc Nothing  = acc =. ent ^. acc
updateHarusIsi _   acc (Just x) = acc =. val x

minumDiTahunBulan
  :: From query SqlExpr backend (SqlExpr (Entity Minum))
  => SqlExpr (Value (Key Meteran))
  -> SqlExpr (Value Int64)
  -> SqlExpr (Value Int)
  -> SqlExpr (Value Int64)
minumDiTahunBulan meteranid tahun bulan = case_
  [ when_
      ( exists $ from $ \minum -> do
        where_ $ minum ^. MinumMeteranId ==. meteranid
        where_ $ minum ^. MinumTahun ==. tahun
        where_ $ minum ^. MinumBulan ==. bulan
      )
      then_
      ( sub_select $ from $ \minum -> do
        where_ $ minum ^. MinumMeteranId ==. meteranid
        where_ $ minum ^. MinumTahun ==. tahun
        where_ $ minum ^. MinumBulan ==. bulan
        return $ minum ^. MinumSampai
      )
  ]
  (else_ $ val 0)

