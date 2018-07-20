{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Air where

import           Protolude

import           Database.Esqueleto

import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Bisnis.PembukuanAir

postCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
postCatatAirPenangan (Authenticated petugas) nomormeteran RequestCatatAir {..} = do
  (tahun, bulan, sampai) <-
    catatAirBulanIni petugas nomormeteran reqcatatairSampai
  return $ ResponsePenggunaanAir nomormeteran tahun bulan sampai
postCatatAirPenangan _ _ _ = throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."

putCatatAirPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> RequestCatatAir
  -> PenanganT m ResponsePenggunaanAir
putCatatAirPenangan (Authenticated petugas) nomormeteran RequestCatatAir {..} = do
  (tahun, bulan, sampai) <-
    ubahCatatanAirBulanIni petugas nomormeteran reqcatatairSampai
  return $ ResponsePenggunaanAir nomormeteran tahun bulan sampai
putCatatAirPenangan _ _ _ = throwError $ GagalTakBerwenang "Tidak boleh mencatat penggunaan air."
