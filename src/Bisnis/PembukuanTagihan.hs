{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bisnis.PembukuanTagihan where

import           Protolude

import           Data.Time
import           Database.Esqueleto

import           Conf
import           Model
import           Model.Grouping
import           Types

import           Bisnis.Otoritas
import           Bisnis.ValidasiData

import           Pertanyaan.TentangTagihan

-- | Melihat daftar tagihan dari semua pengguna.
--   Bila 2 parameter terakhir tidak diisi,
--   maka sistem akan memutuskan untuk melihat
--   tagihan pada tahun dan bulan ini.
--   Bila 2 parameter terakhir bukan merupakan
--   tahun dan bulan yang valid, maka akan melempar
--   `Gagal` yang berhubungan dengan tanggal.
lihatDaftarTagihan
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Pengguna.
  -> Maybe Integer -- ^ Tahun, opsional.
  -> Maybe Int -- ^ Bulan, opsional.
  -> m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         , Value Int64
         )
       ]
lihatDaftarTagihan admin mtahun mbulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  (tini , bini ) <- tahunBulanHarusValid mtahun mbulan
  (tlalu, blalu) <- tahunBulanLalu tini bini
  runDb $ selectDaftarTagihanByTahunBulan Nothing
                                          (fromInteger tini)
                                          bini
                                          (fromInteger tlalu)
                                          blalu

-- | Melakukan pembayaran tagihan atas `Tagihan` dengan nomor
--   meteran di parameter ke dua, pada tahun dan bulan di
--   parameter ke tiga dan empat.
--   Akan melempar galat `Gagal` yang berkaitan dengan tanggal
--   bila parameter ketiga dan keempat tidak valid.
--   Akan melempar galat `GagalTagihanTahunBulanNil` bila tagihan
--   tidak ada.
bayarTagihan
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Admin.
  -> Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun.
  -> Int -- ^ Bulan.
  -> m
       ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       , Value Int64
       )
bayarTagihan admin nometeran tahun bulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  (tini , bini ) <- tahunBulanHarusValid (Just tahun) (Just bulan)
  (tlalu, blalu) <- tahunBulanLalu tini bini
  hariini        <- utctDay <$> liftIO getCurrentTime
  Entity tid _   <- tagihanAda nometeran (fromInteger tini) bini
  tagihan        <- runDb $ do
    updateTagihan tid hariini
    selectDaftarTagihanByTahunBulan (Just nometeran)
                                    (fromInteger tini)
                                    bini
                                    (fromInteger tlalu)
                                    blalu
  case tagihan of
    []  -> throwError $ GagalTagihanTahunBulanNil tini bini
    x:_ -> return x

-- | Melihat tagihan dari meteran dengan nomor
--   sesuai dengan parameter kedua.
--   Bila 2 parameter terakhir tidak diisi,
--   maka sistem akan memutuskan untuk melihat
--   tagihan pada tahun dan bulan ini.
--   Bila 2 parameter terakhir bukan merupakan
--   tahun dan bulan yang valid, maka akan melempar
--   `Gagal` yang berhubungan dengan tanggal.
--   Melempar `GalatTagihanTahunBulanNil` bila meteran
--   tidak memiliki tagihan pada tahun bulan tersebut.
lihatTagihanPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Yang melihat.
  -> Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun.
  -> Int -- ^ Bulan.
  -> m
       ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       , Value Int64
       )
lihatTagihanPengguna admin nometeran tahun bulan = do
  _              <- kewenanganMinimalPengguna admin Admin
  _              <- meteranHarusAda nometeran
  (tini , bini ) <- tahunBulanHarusValid (Just tahun) (Just bulan)
  (tlalu, blalu) <- tahunBulanLalu tini bini
  tagihan        <- runDb $ selectDaftarTagihanByTahunBulan Nothing
                                                            (fromInteger tini)
                                                            bini
                                                            (fromInteger tlalu)
                                                            blalu
  case tagihan of
    []  -> throwError $ GagalTagihanTahunBulanNil tini bini
    x:_ -> return x

-- | Melihat daftar tagihan pengguna dengan nomor meteran pada
--   parameter kedua.
--   Akan melempar `GagalMeteranNil` bila tidak ada meteran
--   dengan nomor sesuai parameter.
lihatDaftarTagihanPengguna
  :: (MonadIO m, MonadReader Konfigurasi m, MonadError Gagal m)
  => Pengguna -- ^ Penglihat.
  -> Text -- ^ Nomor meteran.
  -> m
       [ ( Entity Pengguna
         , Entity Meteran
         , Entity Tagihan
         , Entity Minum
         , Entity Tarif
         )
       ]
lihatDaftarTagihanPengguna admin nometeran = do
  _ <- kewenanganMinimalPengguna admin Admin
  _ <- meteranHarusAda nometeran
  runDb $ selectTagihanPengguna Nothing (Just nometeran)
