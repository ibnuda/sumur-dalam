{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Air
  ( getDaftarMinumPelangganPenangan
  , getMinumPelangganPenangan
  , postCatatAirPenangan
  , putCatatAirPenangan
  ) where

import           Protolude

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.PembukuanAir
import           Bisnis.PembukuanPelanggan

getDaftarMinumPelangganPenangan
  :: MonadIO m => AuthResult Pengguna -> PenanganT m [ResponseDaftarPelanggan]
getDaftarMinumPelangganPenangan (Authenticated petugas) = map f
  <$> lihatPenggunaDanMeteran petugas
 where
  f (Pengguna {..}, Meteran {..}, tercatat) = ResponseDaftarPelanggan
    penggunaNama
    penggunaNomorTelp
    meteranNomor
    penggunaAlamat
    tercatat
getDaftarMinumPelangganPenangan _ =
  throwError $ GagalTakBerwenang "Tidak boleh melihat daftar pelanggan."

getMinumPelangganPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> PenanganT m ResponsePenggunaanAir
getMinumPelangganPenangan (Authenticated petugas) nomormeteran =
  unquadruple ResponsePenggunaanAir
    <$> lihatCatatanAirPelangganBulanIni petugas nomormeteran
getMinumPelangganPenangan _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh melihat daftar pelanggan."

postCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
postCatatAirPenangan (Authenticated p) nomormeteran RequestCatatAir {..} =
  untriple (ResponsePenggunaanAir nomormeteran)
    <$> catatAirBulanIni p nomormeteran reqcatatairSampai
postCatatAirPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."

putCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
putCatatAirPenangan (Authenticated p) nomormeteran RequestCatatAir {..} =
  untriple (ResponsePenggunaanAir nomormeteran)
    <$> ubahCatatanAirBulanIni p nomormeteran reqcatatairSampai
putCatatAirPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."
