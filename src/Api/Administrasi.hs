{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Administrasi where

import           Protolude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Penangan.Sistem

type AdministrasiApi =
  "tarif"
    :> Get '[ JSON] ResponseDataTagihanTarif
  :<|> "tambah"
    :> ReqBody '[ JSON] RequestPelangganBaru
    :> Get '[ JSON] ResponseDataPelanggan
  :<|> "gantipass"
    :> ReqBody '[ JSON] RequestGantiPassword
    :> Put '[ JSON] ResponseDataPelangganToken


administrasiProxy :: Proxy AdministrasiApi
administrasiProxy = Proxy

administrasiApi
  :: MonadIO m => AuthResult Pengguna -> ServerT AdministrasiApi (PenanganT m)
administrasiApi a =
  getTarifTerbaruPenangan a
    :<|> postTambahPelangganPenangan a
    :<|> putGantiPassword a

administrasiServer
  :: Konfigurasi -> AuthResult Pengguna -> Server AdministrasiApi
administrasiServer c a =
  hoistServer administrasiProxy (penanganKeHandler c) (administrasiApi a)
