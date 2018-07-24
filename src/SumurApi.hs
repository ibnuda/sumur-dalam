{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module SumurApi where

import           Protolude

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth
import           Servant.Auth.Server

import           Conf
import           Model
import           Util

import           Api.Administrasi
import           Api.Otorisasi
import           Api.Pembayaran
import           Api.Petugas

type SumurApi =
  OtorisasiApi
    :<|> (Auth '[ JWT] Pengguna :> PetugasApi)
    :<|> (Auth '[ JWT] Pengguna :> AdministrasiApi)
    :<|> (Auth '[ JWT] Pengguna :> PembayaranApi)
    :<|> Raw

sumurProxy :: Proxy SumurApi
sumurProxy = Proxy

sumurServer :: Konfigurasi -> Server SumurApi
sumurServer c =
  otorisasiServer c
    :<|> petugasServer c
    :<|> administrasiServer c
    :<|> pembayaranServer c
    :<|> serveDirectoryFileServer "depan"

connstring :: ByteString
connstring =
  "host=localhost "
    <> "port=5432 "
    <> "user=ibnu "
    <> "password=jaran "
    <> "dbname=kampung"

running :: IO ()
running = do
  jwk   <- readKey "../jaran.key.jwk"
  pool  <- runStderrLoggingT $ createPostgresqlPool connstring 10
  grups <- flip runSqlPool pool $ do
    doMigration
    selectList [] []
  let jws  = defaultJWTSettings jwk
      kuki = defaultCookieSettings { cookieSameSite = AnySite }
      cfg  = kuki :. jws :. EmptyContext
      conf = Konfigurasi pool jws grups
  run 8080
    $ serveWithContext sumurProxy cfg
    $ sumurServer conf
