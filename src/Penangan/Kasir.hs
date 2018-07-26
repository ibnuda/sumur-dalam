{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Kasir where

import           Protolude

import           Database.Esqueleto

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.PembukuanTagihan
import           Bisnis.PembukuanPelanggan

getDaftarDataTagihanPenangan
  :: MonadIO m
  => AuthResult Pengguna -- ^ Hasil otentikasi.
  -> Maybe Integer -- ^ Tahun, opsional.
  -> Maybe Int -- ^ Bulan, opsional.
  -> PenanganT m [ResponseDataTagihan]
getDaftarDataTagihanPenangan (Authenticated admin) mtahun mbulan =
  map (unsextuple querytagihanKeResponse)
    <$> lihatDaftarTagihan admin mtahun mbulan
getDaftarDataTagihanPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Lihat daftar data pengguna."

getTagihanPenggunaTahunBulanPenangan
  :: MonadIO m
  => AuthResult Pengguna -- ^ Hasil otentikasi.
  -> Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun pencatatan.
  -> Int -- ^ Bulan pencatatan.
  -> PenanganT m ResponseDataTagihan
getTagihanPenggunaTahunBulanPenangan (Authenticated admin) nometeran tahun bulan
  = unsextuple querytagihanKeResponse
    <$> lihatTagihanPengguna admin nometeran tahun bulan
getTagihanPenggunaTahunBulanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

getDaftarTagihanPenggunaPenangan
  :: MonadIO m
  => AuthResult Pengguna -- ^ Hasil otentikasi.
  -> Text -- ^ Nomor meteran.
  -> PenanganT m [ResponseDataTagihan]
getDaftarTagihanPenggunaPenangan (Authenticated admin) nometeran =
  querytagihanpenggunaKeResponse <$> lihatDaftarTagihanPengguna admin nometeran
getDaftarTagihanPenggunaPenangan _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

putBayarTagihanPenangan
  :: MonadIO m
  => AuthResult Pengguna -- ^ Hasil otentikasi.
  -> Text -- ^ Nomor meteran.
  -> Integer -- ^ Tahun tagihan.
  -> Int -- ^ Bulan tagihan.
  -> PenanganT m ResponseDataTagihan
putBayarTagihanPenangan (Authenticated a) nometeran tahun bulan =
  unsextuple querytagihanKeResponse <$> bayarTagihan a nometeran tahun bulan
putBayarTagihanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

getDaftarPelangganPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> PenanganT m [ResponseDataPelanggan]
getDaftarPelangganPenangan (Authenticated admin) =
  map f <$> lihatDaftarPelanggan admin
 where
  f (Entity _ Pengguna {..}, Entity _ Meteran {..}) = ResponseDataPelanggan
    penggunaNama
    penggunaNomorTelp
    penggunaAlamat
    penggunaWilayah
    meteranNomor
getDaftarPelangganPenangan _ =
  throwError $ GagalTakBerwenang "Lihat daftar pelanggan."

getRiwayatPelangganPenangan
  :: MonadIO m
  => AuthResult Pengguna -- ^ Hasil otentikasi.
  -> Text -- ^ Nomor meteran.
  -> PenanganT m ResponseRiwayatPelanggan
getRiwayatPelangganPenangan (Authenticated a) nomet = do
  untriple queryriwayatKeResponse <$> lihatRiwayatPelanggan a nomet
getRiwayatPelangganPenangan _ _ =
  throwError $ GagalTakBerwenang "Lihat riwayat pengguna."
