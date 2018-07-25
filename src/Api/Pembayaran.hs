{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Pembayaran where

import           Protolude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Penangan.Kasir

type PembayaranApi =
  "tagihan"
    :> QueryParam "tahun" Integer
    :> QueryParam "bulan" Int
    :> Get '[ JSON] [ResponseDataTagihan]
  :<|> "tagihan"
    :> Capture "nometeran" Text
    :> Get '[ JSON] [ResponseDataTagihan]
  :<|> "tagihan"
    :> Capture "nometeran" Text
    :> Capture "tahun" Integer
    :> Capture "bulan" Int
    :> Get '[ JSON] ResponseDataTagihan
  :<|> "tagihan"
    :> Capture "nometeran" Text
    :> Capture "tahun" Integer
    :> Capture "bulan" Int
    :> PutNoContent '[ JSON] ResponseDataTagihan
  :<|> "pelanggan"
    :> Capture "nometeran" Text
    :> Get '[ JSON] ResponseRiwayatPelanggan


pembayaranProxy :: Proxy PembayaranApi
pembayaranProxy = Proxy

pembayaranApi
  :: MonadIO m => AuthResult Pengguna -> ServerT PembayaranApi (PenanganT m)
pembayaranApi a =
  getDaftarDataTagihanPenangan a
    :<|> getDaftarTagihanPenggunaPenangan a
    :<|> getTagihanPenggunaTahunBulanPenangan a
    :<|> putBayarTagihanPenangan a
    :<|> getRiwayatPelangganPenangan a

pembayaranServer :: Konfigurasi -> AuthResult Pengguna -> Server PembayaranApi
pembayaranServer c a =
  hoistServer pembayaranProxy (penanganKeHandler c) (pembayaranApi a)
