{-# LANGUAGE FlexibleContexts #-}
module Bisnis.ValidasiData
  ( penggunaAda
  , penggunaNil
  , penggunaPunyaMeteran
  , meteranHarusNyala
  , meteranHarusAda
  , meteranHarusNil
  , meteranBulanIniHarusIsi
  , meteranBulanIniHarusKosong
  , tarifTerbaru
  , tahunBulanHarusValid
  , tahunBulanLalu
  , tagihanAda
  ) where

import           Protolude                  hiding (get)

import           Database.Esqueleto

import           Data.Time

import           Conf
import           Model
import           Types

import           Pertanyaan.TentangMinum
import           Pertanyaan.TentangPengguna
import           Pertanyaan.TentangSistem
import           Pertanyaan.TentangTagihan

-- | Memeriksa apakah pengguna dengan nomor telpon di parameter
--   ada pada sistem atau tidak.
--   Akan melempar galat `GagalPenggunaNil text` bila tidak ada.
penggunaAda
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor telpon pengguna.
  -> m (Entity Pengguna)
penggunaAda notelp = do
  mpengguna <- runDb $ getBy $ UniqueNomorTelp notelp
  case mpengguna of
    Nothing -> throwError $ GagalPenggunaNil notelp
    Just x  -> return x

-- | Memeriksa apakah pengguna dengan nomor telpon di parameter
--   ada pada sistem atau tidak.
--   Akan melempar galat `GagalPenggunaNil text` bila tidak ada.
penggunaNil
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor telepon pengguna.
  -> m () -- ^ Kosong.
penggunaNil notelp = do
  mpengguna <- runDb $ getBy $ UniqueNomorTelp notelp
  case mpengguna of
    Nothing -> return ()
    Just _  -> throwError $ GagalPenggunaAda notelp

penggunaPunyaMeteran
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text
  -> m (Entity Meteran)
penggunaPunyaMeteran notelp = do
  meteran <- runDb $ selectMeteranPengguna notelp
  case meteran of
    []  -> throwError $ GagalPenggunaTunaMeteran
    x:_ -> return x

-- | Memeriksa apakah meteran dengan nomor meteran di parameter
--   masih digunakan atau tidak.
--   Akan melempar galat `GagalMeteranNil text` bila tidak ada.
meteranHarusNyala
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor meteran.
  -> m (Entity Meteran)
meteranHarusNyala nometeran = do
  mmeteran <- runDb $ selectMeteran nometeran
  case mmeteran of
    []  -> throwError $ GagalMeteranNil nometeran
    x:_ -> return x

-- | Memeriksa apakah meteran dengan nomor meteran di parameter
--   ada pada sistem atau tidak.
--   Akan melempar galat `GagalMeteranNil text` bila tidak ada.
meteranHarusAda
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text
  -> m (Entity Meteran)
meteranHarusAda nometeran = do
  mmeteran <- runDb $ selectMeteran' nometeran
  case mmeteran of
    []  -> throwError $ GagalMeteranNil nometeran
    x:_ -> return x

-- | Memeriksa apakah meteran dengan nomor meteran di parameter
--   ada pada sistem atau tidak.
--   Akan melempar galat `GagalMeteranNil text` bila tidak ada.
meteranHarusNil
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text
  -> m ()
meteranHarusNil nometeran = do
  mmeteran <- runDb $ selectMeteran' nometeran
  case mmeteran of
    [] -> return ()
    _  -> throwError $ GagalMeteranAda nometeran

-- | Memeriksa tarif terbaru yang ada di sistem
tarifTerbaru
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => m (Entity Tarif)
tarifTerbaru = do
  tarif <- runDb selectTarifTerbaru
  case tarif of
    []  -> throwError GagalTarifKosong
    x:_ -> return x

-- | Memastikan bahwa `Meteran` dengan nomor meteran di parameter
--   belum memiliki dicatat penggunaannya pada tahun dan bulan
--   di parameter.
meteranBulanIniHarusKosong
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> m ()
meteranBulanIniHarusKosong nometeran tahun bulan = do
  minum <- runDb $ selectMinumByNomorMeteran nometeran (Just tahun) (Just bulan)
  case minum of
    [] -> return ()
    _  -> throwError $ GagalMinumAda nometeran

-- | Memastikan bahwa `Meteran` dengan nomor meteran di parameter
--   belum memiliki dicatat penggunaannya pada tahun dan bulan
--   di parameter.
meteranBulanIniHarusIsi
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> m (Entity Meteran, Entity Minum)
meteranBulanIniHarusIsi nometeran tahun bulan = do
  minum <- runDb $ selectMinumByNomorMeteran nometeran (Just tahun) (Just bulan)
  case minum of
    []  -> throwError $ GagalMinumNil nometeran
    x:_ -> return x

-- | Memastikan bahwa tanggal dan bulan yang diinput oleh pengguna
--   sudah lampau atau bukan merupakan tanggal yang valid.
--   Bila pengguna tidak menginput tahun dan bulan, maka sistem
--   akan memberikan tahun dan tanggal saat ini sebagai nilai bawaan.
tahunBulanHarusValid
  :: (MonadError Gagal m, MonadIO m)
  => Maybe Integer -- ^ Input tahun, opsional.
  -> Maybe Int -- ^ Input bulan, opsional.
  -> m (Integer, Int)
tahunBulanHarusValid mtahun mbulan = do
  hari <- utctDay <$> liftIO getCurrentTime
  let (tahun, bulan, h) = toGregorian hari
  case (mtahun, mbulan) of
    (Nothing, Nothing) -> return (tahun, bulan)
    (Just x , Just y ) -> case fromGregorianValid x y h of
      Nothing -> throwError $ GagalTanggalTidakValid x y
      Just d  -> do
        let (t, b, _) = toGregorian d
        if hari < d
          then throwError $ GagalTanggalBelumAda t b
          else return (t, b)
    _ -> throwError GagalTanggalTidakAda

tahunBulanLalu
  :: (MonadIO m, MonadError Gagal m) => Integer -> Int -> m (Integer, Int)
tahunBulanLalu t b = do
  (tahun, bulan) <- tahunBulanHarusValid (Just t) (Just b)
  let haripertama       = fromGregorian tahun bulan 1
      hariterakhir      = addDays (-1) haripertama
      (tlalu, blalu, _) = toGregorian hariterakhir
  return (tlalu, blalu)

tagihanAda
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Text
  -> Int64
  -> Int
  -> m (Entity Tagihan)
tagihanAda nometeran tahun bulan = do
  tagihan <- runDb $ selectTagihan nometeran tahun bulan
  case tagihan of
    []  -> throwError $ GagalTagihanNil nometeran tahun bulan
    x:_ -> return x
