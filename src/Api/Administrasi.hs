{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Api.Administrasi where

import           Protolude

import           Servant
import           Servant.Auth.Server
import           Servant.Foreign

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

instance forall lang ftype. ( HasForeign lang ftype AdministrasiApi
                            , HasForeignType lang ftype Text
                            , GenerateList NoContent (Foreign NoContent AdministrasiApi)
         ) =>
         HasForeign lang ftype (Auth '[ JWT] Pengguna :> AdministrasiApi) where
  type Foreign ftype (Auth '[ JWT] Pengguna :> AdministrasiApi) = Foreign ftype AdministrasiApi
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy AdministrasiApi) req
    where
      req = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }

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
