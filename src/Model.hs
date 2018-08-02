{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH
import           Servant.Auth.Server

import           Model.Grouping

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Grup
      nama GrupSistem
      UniqueGrup nama
      deriving Show Eq Read
    Pengguna
      nama Text
      nomorTelp Text
      password Text
      grupId GrupId
      alamat Text
      wilayah Text
      UniqueNomorTelp nomorTelp
      deriving Generic
    Meteran
      penggunaId PenggunaId
      nomor Text
      tanggalDaftar Day
      tanggalPutus Day Maybe
      UniqueNomor nomor
      deriving Generic
    Minum
      meteranId MeteranId
      petugasId PenggunaId
      tahun Int
      bulan Int
      sampai Int
      UniqueMeteranTahunBulan meteranId tahun bulan
      deriving Generic
    Tarif
      hargaAwal Int64
      sampaiAwal Int64
      hargaTengah Int64
      sampaiTengah Int64
      hargaAkhir Int64
      biayaBeban Int64
      deriving Generic
    Tagihan
      minumId MinumId
      tarifId TarifId
      tanggalBayar Day Maybe
  |]

instance ToJSON Pengguna where
  toJSON = genericToJSON (aesonPrefix snakeCase)
instance FromJSON Pengguna where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)
instance ToJWT Pengguna
instance FromJWT Pengguna

doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll
