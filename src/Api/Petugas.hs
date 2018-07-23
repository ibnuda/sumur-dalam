{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Api.Petugas where

import           Protolude

import           Servant
import           Servant.Auth.Server
import           Servant.Foreign

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

instance forall lang ftype. ( HasForeign lang ftype PetugasApi
                            , HasForeignType lang ftype Text
                            , GenerateList NoContent (Foreign NoContent PetugasApi)
         ) =>
         HasForeign lang ftype (Auth '[ JWT] Pengguna :> PetugasApi) where
  type Foreign ftype (Auth '[ JWT] Pengguna :> PetugasApi) = Foreign ftype PetugasApi
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy PetugasApi) req
    where
      req = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }

petugasProxy :: Proxy PetugasApi
petugasProxy = Proxy

petugasApi
  :: MonadIO m => AuthResult Pengguna -> ServerT PetugasApi (PenanganT m)
petugasApi a =
  getDaftarPelangganPenangan a
    :<|> getMinumPelangganPenangan a
    :<|> postCatatAirPenangan a
    :<|> putCatatAirPenangan a

petugasServer :: Konfigurasi -> AuthResult Pengguna -> Server PetugasApi
petugasServer c a =
  hoistServer petugasProxy (penanganKeHandler c) (petugasApi a)
