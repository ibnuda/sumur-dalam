{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Kasir where

import           Protolude

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.PembukuanTagihan

getDaftarDataTagihanPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Maybe Integer
  -> Maybe Int
  -> PenanganT m [ResponseDataTagihan]
getDaftarDataTagihanPenangan (Authenticated admin) mtahun mbulan =
  map (unsextuple querytagihanKeResponse)
    <$> lihatDaftarTagihan admin mtahun mbulan
getDaftarDataTagihanPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Lihat daftar data pengguna."

getTagihanPenggunaTahunBulanPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> Integer
  -> Int
  -> PenanganT m ResponseDataTagihan
getTagihanPenggunaTahunBulanPenangan (Authenticated admin) nometeran tahun bulan
  = unsextuple querytagihanKeResponse
    <$> lihatTagihanPengguna admin nometeran tahun bulan
getTagihanPenggunaTahunBulanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

getDaftarTagihanPenggunaPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> PenanganT m [ResponseDataTagihan]
getDaftarTagihanPenggunaPenangan (Authenticated admin) nometeran =
  querytagihanpenggunaKeResponse <$> lihatDaftarTagihanPengguna admin nometeran
getDaftarTagihanPenggunaPenangan _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

putBayarTagihanPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> Integer
  -> Int
  -> PenanganT m ResponseDataTagihan
putBayarTagihanPenangan (Authenticated a) notelp tahun bulan =
  unsextuple querytagihanKeResponse <$> bayarTagihan a notelp tahun bulan
putBayarTagihanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."
