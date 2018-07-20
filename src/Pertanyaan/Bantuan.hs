module Pertanyaan.Bantuan where

import           Protolude

import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language

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
