{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Petugas where

import           Protolude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Penangan.Air

type PetugasApi =
  "petugas"
    :> "air"
    :> Get '[ JSON] [ResponseDaftarPelanggan]
  :<|> "petugas"
    :> "air"
    :> Capture "nomormeteran" Text
    :> Get '[ JSON] ResponsePenggunaanAir
  :<|> "petugas"
    :> "air"
    :> Capture "nomormeteran" Text
    :> ReqBody '[ JSON] RequestCatatAir
    :> Post '[ JSON] ResponsePenggunaanAir
  :<|> "petugas"
    :> "air"
    :> Capture "nomormeteran" Text
    :> ReqBody '[ JSON] RequestCatatAir
    :> Put '[ JSON] ResponsePenggunaanAir

petugasProxy :: Proxy PetugasApi
petugasProxy = Proxy

petugasApi
  :: MonadIO m => AuthResult Pengguna -> ServerT PetugasApi (PenanganT m)
petugasApi a =
  getDaftarMinumPelangganPenangan a
    :<|> getMinumPelangganPenangan a
    :<|> postCatatAirPenangan a
    :<|> putCatatAirPenangan a

petugasServer :: Konfigurasi -> AuthResult Pengguna -> Server PetugasApi
petugasServer c a =
  hoistServer petugasProxy (penanganKeHandler c) (petugasApi a)
