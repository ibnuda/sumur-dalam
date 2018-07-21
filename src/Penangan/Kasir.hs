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
getDaftarDataTagihanPenangan (Authenticated admin) mtahun mbulan = do
  map (unsextuple querytagihanKeResponse)
    <$> lihatDaftarTagihan admin mtahun mbulan
getDaftarDataTagihanPenangan _ _ _ =
  throwError $ GagalTakBerwenang "Lihat daftar data pengguna."

getTagihanPenggunaTahunBulanPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> Text
  -> Maybe Integer
  -> Maybe Int
  -> PenanganT m ResponseDataTagihan
getTagihanPenggunaTahunBulanPenangan (Authenticated admin) notelp mtahun mbulan = do
  unsextuple querytagihanKeResponse <$> lihatTagihanPengguna admin notelp mtahun mbulan
getTagihanPenggunaTahunBulanPenangan _ _ _ _ =
  throwError $ GagalTakBerwenang "Lihat tagihan pengguna."
