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
getTagihanPenggunaTahunBulanPenangan (Authenticated admin) notelp tahun bulan =
  unsextuple querytagihanKeResponse
    <$> lihatTagihanPengguna admin notelp tahun bulan
getTagihanPenggunaTahunBulanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

getDaftarTagihanPenggunaPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> PenanganT m [ResponseDataTagihan]
getDaftarTagihanPenggunaPenangan (Authenticated admin) notelp =
  querytagihanpenggunaKeResponse <$> lihatDaftarTagihanPengguna admin notelp
getDaftarTagihanPenggunaPenangan _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."

putBayarTagihanPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> Int64
  -> PenanganT m ResponseDataTagihan
putBayarTagihanPenangan (Authenticated a) notelp tid =
  unsextuple querytagihanKeResponse <$> bayarTagihan a notelp tid
putBayarTagihanPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."
