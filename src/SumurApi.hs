{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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

import           Api.Otorisasi
import           Api.Petugas
import           Api.Administrasi
import           Api.Pembayaran

type SumurApi a =
  OtorisasiApi
    :<|> (Auth a Pengguna :> PetugasApi)
    :<|> (Auth a Pengguna :> AdministrasiApi)
    :<|> (Auth a Pengguna :> PembayaranApi)
    :<|> Raw

sumurProxy :: Proxy (SumurApi '[JWT])
sumurProxy = Proxy

sumurServer :: Konfigurasi -> Server (SumurApi a)
sumurServer c =
  otorisasiServer c
    :<|> petugasServer c
    :<|> administrasiServer c
    :<|> pembayaranServer c
    :<|> serveDirectoryFileServer ""

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
      cfg  = defaultCookieSettings :. jws :. EmptyContext
      conf = Konfigurasi pool jws grups
  run 8080 $ serveWithContext sumurProxy cfg $ sumurServer conf
