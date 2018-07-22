{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.Otoritas
  ( otentikasi
  , kewenanganMinimalPengguna
  , gantiPassword
  ) where

import           Protolude

import           Crypto.BCrypt

import           Database.Esqueleto

import           Conf
import           Model
import           Model.Grouping
import           Types
import           Util

import           Pertanyaan.TentangPengguna

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
  mpengguna <- mungkinPengguna notelp
  buatTokenAtauGagal (validasiPenggunaPassword mpengguna passw) passw
 where
  mungkinPengguna
    :: (MonadReader Konfigurasi m, MonadIO m)
    => Text
    -> m (Maybe (Entity Pengguna))
  mungkinPengguna tlp = runDb $ getBy $ UniqueNomorTelp tlp

  validasiPenggunaPassword
    :: Maybe (Entity Pengguna) -> Text -> Either Gagal Pengguna
  validasiPenggunaPassword (Just (Entity _ Pengguna {..})) pw =
    case (validatePassword (encodeUtf8 penggunaPassword) (encodeUtf8 pw)) of
      True  -> Right Pengguna {..}
      False -> Left GagalMasuk
  validasiPenggunaPassword Nothing _ = Left GagalMasuk

  buatTokenAtauGagal
    :: (MonadReader Konfigurasi m, MonadIO m, MonadError Gagal m)
    => Either Gagal Pengguna
    -> Text
    -> m (Pengguna, Text)
  buatTokenAtauGagal (Left err) pw = do
    _ <- liftIO $ buatPassword pw
    throwError err
  buatTokenAtauGagal (Right p) _ = do
    token <- buatToken p
    return (p, token)

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

gantiPassword
  :: (MonadReader Konfigurasi m, MonadIO m, MonadError Gagal m)
  => Pengguna
  -> Text
  -> Text
  -> m (Pengguna, Text)
gantiPassword Pengguna {..} passlama passbaru = do
  unless (validatePassword (encodeUtf8 penggunaPassword) (encodeUtf8 passlama)) $
    throwError $ GagalTakBerwenang "Password lama beda."
  hashpassbaru <- liftIO $ buatPassword passbaru
  pengguna <- runDb $ do
    updatePengguna penggunaNomorTelp Nothing (Just hashpassbaru) Nothing Nothing Nothing
    selectPenggunaByNomorTelepon penggunaNomorTelp
  case pengguna of
    [] -> throwError $ GagalDB "Ganti password" "Pengguna tidak ada."
    x:_ -> do
      token <- buatToken $ entityVal x
      return (entityVal x, token)
