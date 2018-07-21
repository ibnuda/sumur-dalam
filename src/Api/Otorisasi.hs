{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.Otorisasi
  ( OtorisasiApi
  , otorisasiServer
  ) where

import           Protolude

import           Servant

import           Conf
import           Types

import           Penangan.Pintu

type OtorisasiApi =
  "masuk"
    :> ReqBody '[ JSON] RequestMasuk
    :> Post '[ JSON] ResponseDataPelangganToken

otorisasiProxy :: Proxy OtorisasiApi
otorisasiProxy = Proxy

otorisasiApi :: MonadIO m => ServerT OtorisasiApi (PenanganT m)
otorisasiApi = postMasukPenangan

otorisasiServer :: Konfigurasi -> Server OtorisasiApi
otorisasiServer k =
  hoistServer otorisasiProxy (penanganKeHandler k) otorisasiApi
