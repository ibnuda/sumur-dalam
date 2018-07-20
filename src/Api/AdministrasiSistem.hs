{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Api.AdministrasiSistem where

import           Protolude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

type AdministrasiSistemApi =
  "sistem"
    :> Get '[ JSON] NoContent

administrasiSistemProxy :: Proxy AdministrasiSistemApi
administrasiSistemProxy = Proxy

administrasiSistemApi
  :: MonadIO m
  => AuthResult Pengguna
  -> ServerT AdministrasiSistemApi (PenanganT m)
administrasiSistemApi _ = panic "belum dibuat"

administrasiSistemServer
  :: Konfigurasi -> AuthResult Pengguna -> Server AdministrasiSistemApi
administrasiSistemServer c a = hoistServer administrasiSistemProxy
                                           (penanganKeHandler c)
                                           (administrasiSistemApi a)
