{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Air where

import           Protolude

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.PembukuanAir
import           Bisnis.PembukuanPelanggan

getDaftarPelangganPenangan
  :: MonadIO m => AuthResult Pengguna -> PenanganT m [ResponseDaftarPelanggan]
getDaftarPelangganPenangan (Authenticated petugas) = map f
  <$> lihatPenggunaDanMeteran petugas
 where
  f (Pengguna {..}, Meteran {..}, tercatat) = ResponseDaftarPelanggan
    penggunaNama
    penggunaNomorTelp
    meteranNomor
    penggunaAlamat
    tercatat
getDaftarPelangganPenangan _ =
  throwError $ GagalTakBerwenang "Tidak boleh melihat daftar pelanggan."

getMinumPelangganPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> PenanganT m ResponsePenggunaanAir
getMinumPelangganPenangan (Authenticated petugas) nomormeteran =
  unquad ResponsePenggunaanAir <$> lihatCatatanAirPelangganBulanIni petugas nomormeteran
getMinumPelangganPenangan _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh melihat daftar pelanggan."

postCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
postCatatAirPenangan (Authenticated petugas) nomormeteran RequestCatatAir {..} =
  untriad (ResponsePenggunaanAir nomormeteran) <$>
  catatAirBulanIni petugas nomormeteran reqcatatairSampai
postCatatAirPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."

putCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
putCatatAirPenangan (Authenticated petugas) nomormeteran RequestCatatAir {..} =
  do
    (tahun, bulan, sampai) <- ubahCatatanAirBulanIni petugas
                                                     nomormeteran
                                                     reqcatatairSampai
    return $ ResponsePenggunaanAir nomormeteran tahun bulan sampai
putCatatAirPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."
