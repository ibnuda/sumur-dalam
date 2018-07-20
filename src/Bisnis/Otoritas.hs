{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.Otoritas where

import           Protolude

import           Crypto.BCrypt

import           Data.ByteString.Char8 (pack)
import           Data.Text             (unpack)

import           Database.Esqueleto

import           Conf
import           Model
import           Model.Grouping
import           Types
import           Util

-- | Digunakan untuk memberikan aksis otorisasi ke sistem.
--   Ketika pengguna tidak ada, sistem akan melakukan hashing password
--   yang tidak akan digunakan dengan alasan keamanan.
--   (memperlambat bruteforce)
otentikasi
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor telepon dari request.
  -> Text -- ^ Password dari request.
  -> m (Pengguna, Text) -- ^ Teks merupakan token JWT.
otentikasi notelp passw = do
  mpengguna <- runDb $ getBy $ UniqueNomorTelp notelp
  case mpengguna of
    Nothing -> do
      _ <- liftIO $ buatPassword passw
      throwError GagalMasuk
    Just (Entity _ Pengguna {..}) -> do
      unless (validatePassword (punp penggunaPassword) (punp passw))
             (throwError GagalMasuk)
      token <- buatToken Pengguna {..}
      return (Pengguna {..}, token)
 where
  punp :: Text -> ByteString
  punp = pack . unpack

-- | Melakukan pemeriksaan kewenangan yang dimiliki oleh pengguna
--   berdasarkan otorisasi JWT terhadap grup yang ada di sistem.
--   Bila grup tidak dikenal, maka akan melempar galat `GagalDB x y`
--   atau `GalatTakBerwenang` ketika kewenangan lebih rendah dari
--   parameter kedua pada fungsi ini.
kewenanganMinimalPengguna
  :: (MonadError Gagal m, MonadReader Konfigurasi m)
  => Pengguna -- ^ Pengguna berdasarkan token JWT.
  -> GrupSistem -- ^ Grup pembanding.
  -> m Pengguna
kewenanganMinimalPengguna Pengguna {..} grup = do
  grupada <- asks konfigurasiGrupYangAda
  case find (\x -> penggunaGrupId == entityKey x) grupada of
    Nothing -> throwError $ GagalDB "Grup tidak ada" "Cek kewenangan saat ini."
    Just (Entity _ (Grup y))
      | y <= grup -> return Pengguna {..}
      | otherwise -> throwError
      $  GagalTakBerwenang "Tidak berhak melakukan hal tersebut."
