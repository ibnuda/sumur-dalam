{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Penangan.Sistem where

import           Protolude

import           Database.Esqueleto

import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Bisnis.LainLain
import           Bisnis.PembukuanPelanggan
import           Bisnis.Otoritas

getTarifTerbaruPenangan
  :: MonadIO m => AuthResult Pengguna -> PenanganT m ResponseDataTagihanTarif
getTarifTerbaruPenangan (Authenticated x) = do
  tarifKeResponse <$> lihatTarifTerbaru x
getTarifTerbaruPenangan _ = throwError $ GagalTakBerwenang "Tidak boleh lihat."

postTambahPelangganPenangan
  :: MonadIO m
  => AuthResult Pengguna
  -> RequestPelangganBaru
  -> PenanganT m ResponseDataPelanggan
postTambahPelangganPenangan (Authenticated admin) RequestPelangganBaru {..} =
  f
    <$> tambahPengguna admin
                       rpbNama
                       rpbNomorTelepon
                       rpbPassword
                       rpbAlamat
                       rpbWilayah
                       rpbNomorMeteran
 where
  f (Entity _ Pengguna {..}, Entity _ Meteran {..}) = ResponseDataPelanggan
    penggunaNama
    penggunaNomorTelp
    penggunaAlamat
    penggunaWilayah
    meteranNomor
postTambahPelangganPenangan _ _ =
  throwError $ GagalTakBerwenang "Menambah pelanggan."

putGantiPassword
  :: MonadIO m
  => AuthResult Pengguna
  -> RequestGantiPassword
  -> PenanganT m ResponseDataPelangganToken
putGantiPassword (Authenticated pengguna) RequestGantiPassword {..} =
  untuple f <$> gantiPassword pengguna rgpPassLama rgpPassBaru
 where
  f Pengguna {..} token = ResponseDataPelangganToken
    penggunaNama
    penggunaNomorTelp
    (fromSqlKey penggunaGrupId)
    token
putGantiPassword _ _ = throwError $ GagalTakBerwenang "Ganti password."
