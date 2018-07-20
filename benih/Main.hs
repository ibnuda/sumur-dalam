{-# LANGUAGE FlexibleContexts #-}

import           Protolude

import           Control.Monad.Logger
import           Database.Persist.Postgresql

import           Model
import           Model.Grouping

import           Qu.Pengguna
import           Qu.Tarif

connstring :: ByteString
connstring =
  "host=localhost "
    <> "port=5432 "
    <> "user=ibnu "
    <> "password=jaran "
    <> "dbname=kampung"

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool connstring 10
  runSqlPool doMigration pool
  putText "nama: "
  nama     <- getLine
  putText "telepon: "
  telepon  <- getLine
  putText "password: "
  password <- getLine
  putText "alamat: "
  alamat   <- getLine
  flip runSqlPool pool $ do
    _ <- insertGrup SuperAdmin
    _ <- insertGrup Admin
    _ <- insertGrup Petugas
    _ <- insertGrup Pelanggan
    _ <- insertTarif 0 10 1000 20 1000
    insertPengguna nama telepon password SuperAdmin alamat
