{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Api.Pembayaran where

import           Protolude

import           Servant
import           Servant.Auth.Server
import           Servant.Foreign

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

instance forall lang ftype. ( HasForeign lang ftype PembayaranApi
                            , HasForeignType lang ftype Text
                            , GenerateList NoContent (Foreign NoContent PembayaranApi)
         ) =>
         HasForeign lang ftype (Auth '[ JWT] Pengguna :> PembayaranApi) where
  type Foreign ftype (Auth '[ JWT] Pengguna :> PembayaranApi) = Foreign ftype PembayaranApi
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy PembayaranApi) req
    where
      req = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }

pembayaranProxy :: Proxy PembayaranApi
pembayaranProxy = Proxy

pembayaranApi
  :: MonadIO m => AuthResult Pengguna -> ServerT PembayaranApi (PenanganT m)
pembayaranApi a =
  getDaftarDataTagihanPenangan a
    :<|> getDaftarTagihanPenggunaPenangan a
    :<|> getTagihanPenggunaTahunBulanPenangan a
    :<|> putBayarTagihanPenangan a

pembayaranServer :: Konfigurasi -> AuthResult Pengguna -> Server PembayaranApi
pembayaranServer c a =
  hoistServer pembayaranProxy (penanganKeHandler c) (pembayaranApi a)
