{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.AdministrasiSistem
  ( AdministrasiSistemApi
  , administrasiSistemServer
  ) where

import           Protolude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Penangan.Sistem

type AdministrasiSistemApi =
  "sistem"
    :> Get '[ JSON] NoContent
  :<|> "tarif"
    :> Get '[ JSON] ResponseDataTagihanTarif
  :<|> "ganti"
    :> ReqBody '[ JSON] RequestPelangganBaru
    :> Get '[ JSON] NoContent

administrasiSistemProxy :: Proxy AdministrasiSistemApi
administrasiSistemProxy = Proxy

administrasiSistemApi
  :: MonadIO m
  => AuthResult Pengguna
  -> ServerT AdministrasiSistemApi (PenanganT m)
administrasiSistemApi a = panic "belum dibuat" :<|> getTarifTerbaruPenangan a :<|> panic ""

administrasiSistemServer
  :: Konfigurasi -> AuthResult Pengguna -> Server AdministrasiSistemApi
administrasiSistemServer c a = hoistServer administrasiSistemProxy
                                           (penanganKeHandler c)
                                           (administrasiSistemApi a)
