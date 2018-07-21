{-# LANGUAGE FlexibleContexts #-}

import           Protolude

import           Control.Monad.Logger
import           Database.Persist.Postgresql

import           Model
import           Model.Grouping

import           Pertanyaan.TentangPengguna
import           Pertanyaan.TentangSistem

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
  putStr ("nama: " :: [Char])
  nama <- getLine
  putStr ("telepon: " :: [Char])
  telepon <- getLine
  putStr ("password: " :: [Char])
  password <- getLine
  putStr ("alamat: " :: [Char])
  alamat <- getLine
  _      <- flip runSqlPool pool $ do
    _ <- insertGrup SuperAdmin
    _ <- insertGrup Admin
    _ <- insertGrup Petugas
    _ <- insertGrup Pelanggan
    _ <- insertTarif 0 10 1000 20 1000 20000
    insertPengguna nama telepon password SuperAdmin alamat
  putText "Nice."
