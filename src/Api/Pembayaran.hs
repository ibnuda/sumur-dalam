{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Pembayaran
  ( PembayaranApi
  , pembayaranServer
  ) where

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
    :> Capture "nomortelp" Text
    :> QueryParam "tahun" Integer
    :> QueryParam "bulan" Int
    :> Get '[ JSON] ResponseDataTagihan

pembayaranProxy :: Proxy PembayaranApi
pembayaranProxy = Proxy

pembayaranApi
  :: MonadIO m => AuthResult Pengguna -> ServerT PembayaranApi (PenanganT m)
pembayaranApi a =
  getDaftarDataTagihanPenangan a
  :<|> getTagihanPenggunaTahunBulanPenangan a

pembayaranServer :: Konfigurasi -> AuthResult Pengguna -> Server PembayaranApi
pembayaranServer c a =
  hoistServer pembayaranProxy (penanganKeHandler c) (pembayaranApi a)
