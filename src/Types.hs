{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Types where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (pack)
import           Servant.Server

import           Model.Grouping

omitsnake :: Options
omitsnake =
  let snake = aesonPrefix snakeCase in snake { omitNothingFields = True }

data RequestMasuk = RequestMasuk
  { reqmasukNomorTelepon :: Text
  , reqmasukPassword     :: Text
  } deriving (Generic)
instance FromJSON RequestMasuk where
  parseJSON = genericParseJSON omitsnake

data RequestCatatAir = RequestCatatAir
  { reqcatatairSampai       :: Int64
  } deriving (Generic)
instance FromJSON RequestCatatAir where
  parseJSON = genericParseJSON omitsnake

data ResponseDataPelangganToken = ResponseDataPelangganToken
  { respdptNama   :: Text
  , respdptTelp   :: Text
  , respdptIdGrup :: Int64
  , respdptToken  :: Text
  } deriving (Generic)
instance ToJSON ResponseDataPelangganToken where
  toJSON = genericToJSON omitsnake

data ResponsePenggunaanAir = ResponsePenggunaanAir
  { resppaNomorMeteran :: Text
  , resppaTahun        :: Integer
  , resppaBulan        :: Int
  , resppaSampai       :: Int64
  } deriving (Generic)
instance ToJSON ResponsePenggunaanAir where
  toJSON = genericToJSON omitsnake

data Gagal
  = GagalMasuk
  | GagalAdminNil { gadminnilPid :: Int64 }
  | GagalBayar { gbayarAlasan :: Text }
  | GagalCatatAir { gcatatairAlasan       :: Text
                  , gcatatairNomorMeteran :: Text }
  | GagalDB { gdbAlasan :: Text
            , gdbAksi   :: Text }
  | GagalDataTidakAda
  | GagalMeteranAda { gmeteranadaNomor :: Text }
  | GagalMeteranNil { gmeterannilNomor :: Text }
  | GagalMinumAda { gminumadaNomor :: Text }
  | GagalMinumNil { gminumnilNomor :: Text }
  | GagalPasswordBeda
  | GagalPenggunaAda { gpenggunaadaTelepon :: Text }
  | GagalPenggunaNil { gpenggunanilTelepon :: Text }
  | GagalPenggunaTunaMeteran
  | GagalTakBerwenang { gtakwenangSaat :: Text }
  | GagalTambahPelanggan { gtambahpelAlasan :: Text
                         , gtambahpelDetail :: Text }
  | GagalTanggalBelumAda { gtglbTahun :: Integer
                         , gtglbBulan :: Int }
  | GagalTanggalTidakValid { gtgltTahun :: Integer
                           , gtgltBulan :: Int }
  | GagalTanggalTidakAda
  | GagalTarifKosong
  | GagalUbahAir { gubahairAlasan       :: Text
                 , gubahairNomorMeteran :: Text }
  | GagalUpdatePassword { gupdatepassAlasan :: Text }
  deriving (Show, Eq)

data RespError = RespError
  { resperrErrors :: Text
  } deriving (Generic)
instance ToJSON RespError where
  toJSON = genericToJSON omitsnake

encodeRespError :: Text -> BL.ByteString
encodeRespError = encode . RespError

gagalToServantErr :: Gagal -> ServantErr
gagalToServantErr GagalMasuk =
  err400 { errBody = encodeRespError "nomor telepon atau password salah" }
gagalToServantErr (GagalAdminNil n) = err404
  { errBody = encodeRespError
    $  "nomor "
    <> (pack . show $ n)
    <> " bukan admin."
  }
gagalToServantErr (GagalBayar y) = err404 { errBody = encodeRespError y }
gagalToServantErr (GagalCatatAir a n) =
  err404 { errBody = encodeRespError (a <> " di nomor " <> n) }
gagalToServantErr (GagalDB a x) =
  err500 { errBody = encodeRespError (a <> " saat " <> x) }
gagalToServantErr GagalDataTidakAda =
  err404 { errBody = encodeRespError "Data kosong." }
gagalToServantErr (GagalMeteranAda n) =
  err422 { errBody = encodeRespError ("nomor " <> n <> " sudah dipakai.") }
gagalToServantErr (GagalMeteranNil n) =
  err404 { errBody = encodeRespError ("nomor " <> n <> " tidak dipakai.") }
gagalToServantErr (GagalMinumAda n) =
  err422 { errBody = encodeRespError $ n <> " sudah dicatat." }
gagalToServantErr (GagalMinumNil n) =
  err404 { errBody = encodeRespError $ n <> " belum dicatat." }
gagalToServantErr (GagalPenggunaAda n) =
  err422 { errBody = encodeRespError ("nomor " <> n <> " sudah dipakai.") }
gagalToServantErr (GagalPenggunaNil n) =
  err404 { errBody = encodeRespError ("nomor " <> n <> " tidak dipakai.") }
gagalToServantErr (GagalTakBerwenang saat) =
  err401 { errBody = encodeRespError saat }
gagalToServantErr (GagalTambahPelanggan a d) =
  err401 { errBody = encodeRespError (a <> "saat " <> d) }
gagalToServantErr GagalTanggalTidakAda =
  err400{ errBody = encodeRespError "harap isi tanggal." }
gagalToServantErr (GagalTanggalBelumAda t b) = err400
  { errBody = encodeRespError
    $  (pack . show $ t)
    <> " "
    <> (pack . show $ b)
    <> " belum terjadi."
  }
gagalToServantErr (GagalTanggalTidakValid t b) = err400
  { errBody = encodeRespError
    $  (pack . show $ t)
    <> " "
    <> (pack . show $ b)
    <> " tidak valid."
  }
gagalToServantErr (GagalUbahAir a n) =
  err422 { errBody = encodeRespError (a <> " di nomor " <> n) }
gagalToServantErr (GagalUpdatePassword a) =
  err400 { errBody = encodeRespError a }
gagalToServantErr GagalPasswordBeda =
  err401 { errBody = encodeRespError "Password beda." }
gagalToServantErr GagalPenggunaTunaMeteran =
  err404 { errBody = encodeRespError "Ybs tidak punya meteran." }
gagalToServantErr GagalTarifKosong =
  err404 { errBody = encodeRespError "Tarif belum ditentukan." }
gagalToServantErr _ = err500 { errBody = encodeRespError "Mohon bilang ke Ibnu."}

instance Exception Gagal
