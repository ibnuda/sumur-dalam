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
import           Data.Time

import           Servant.Server

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

data RequestTarifBaru = RequestTarifBaru
  { rtbAwalHarga    :: Int64
  , rtbAwalSampai   :: Int64
  , rtbTengahHarga  :: Int64
  , rtbTengahSampai :: Int64
  , rtbAkhirHarga   :: Int64
  , rtbBiayaBeban   :: Int64
  } deriving (Generic)
instance FromJSON RequestTarifBaru where
  parseJSON = genericParseJSON omitsnake

data RequestPelangganBaru = RequestPelangganBaru
  { rpbNama         :: Text
  , rpbNomorTelepon :: Text
  , rpbPassword     :: Text
  , rpbAlamat       :: Text
  , rpbWilayah      :: Text
  , rpbNomorMeteran :: Text
  } deriving (Generic)
instance FromJSON RequestPelangganBaru where
  parseJSON = genericParseJSON omitsnake

data RequestGantiPassword = RequestGantiPassword
  { rgpPassLama :: Text
  , rgpPassBaru :: Text
  } deriving (Generic)
instance FromJSON RequestGantiPassword where
  parseJSON = genericParseJSON omitsnake

data ResponseDataPelangganToken = ResponseDataPelangganToken
  { respdptNama   :: Text
  , respdptTelp   :: Text
  , respdptIdGrup :: Int64
  , respdptToken  :: Text
  } deriving (Generic)
instance ToJSON ResponseDataPelangganToken where
  toJSON = genericToJSON omitsnake

data ResponseDataPelanggan = ResponseDataPelanggan
  { rpNamaPelanggan :: Text
  , rpNomorTelepon  :: Text
  , rpAlamat        :: Text
  , rpWilayah       :: Text
  , rpNomorMeteran  :: Text
  } deriving (Generic)
instance ToJSON ResponseDataPelanggan where
  toJSON = genericToJSON omitsnake

data ResponsePenggunaanAir = ResponsePenggunaanAir
  { resppaNomorMeteran :: Text
  , resppaTahun        :: Integer
  , resppaBulan        :: Int
  , resppaSampai       :: Int64
  } deriving (Generic)
instance ToJSON ResponsePenggunaanAir where
  toJSON = genericToJSON omitsnake

data ResponseDaftarPelanggan = ResponseDaftarPelanggan
  { rdpNamaPelanggan :: Text
  , rdpNomorTelepon  :: Text
  , rdpNomorMeteran  :: Text
  , rdpAlamat        :: Text
  , rdpSudahCatat    :: Bool
  } deriving (Generic)
instance ToJSON ResponseDaftarPelanggan where
  toJSON = genericToJSON omitsnake

data ResponseDataTagihanMeteran = ResponseDataTagihanMeteran
  { rdtmBulanLalu  :: Int64
  , rdtmBulanIni   :: Int64
  , rdtmPenggunaan :: Int64
  } deriving (Generic)
instance ToJSON ResponseDataTagihanMeteran where
  toJSON = genericToJSON omitsnake

--data ResponseDataTagihanTarifItem = ResponseDataTagihanTarifItem
--  { rdttiMulai  :: Int64
--  , rdttiSampai :: Maybe Int64
--  , rdttiHarga  :: Int64
--  } deriving (Generic)
--instance ToJSON ResponseDataTagihanTarifItem where
--  toJSON = genericToJSON omitsnake

--data ResponseDataTagihanTarif = ResponseDataTagihanTarif
--  { rdttSatuan     :: [ResponseDataTagihanTarifItem]
--  , rdttBiayaBeban :: Int64
--  } deriving (Generic)
--instance ToJSON ResponseDataTagihanTarif where
--  toJSON = genericToJSON omitsnake

data ResponseDataTagihanTarif = ResponseDataTagihanTarif
  { rtdHargaAwal    :: Int64
  , rtdSampaiAwal   :: Int64
  , rtdHargaTengah  :: Int64
  , rtdSampaiTengah :: Int64
  , rtdHargaAkhir   :: Int64
  , rtdBiayaBeban   :: Int64
  } deriving (Generic)
instance ToJSON ResponseDataTagihanTarif where
  toJSON = genericToJSON omitsnake

data ResponseDataTagihanPengguna = ResponseDataTagihanPengguna
  { rtdpNomorPelanggan :: Int64
  , rdtpNamaPelanggan  :: Text
  , rdtpNomorTelepon   :: Text
  , rtdpAlamat         :: Text
  , rtdpWilayah        :: Text
  } deriving (Generic)
instance ToJSON ResponseDataTagihanPengguna where
  toJSON = genericToJSON omitsnake

data ResponseDataTagihan = ResponseDataTagihan
  { rdtNomorTagihan  :: Int64
  , rtdNomorMeteran  :: Text
  , rdtTahun         :: Int64
  , rdtBulan         :: Int
  , rdtPengguna      :: ResponseDataTagihanPengguna
  , rdtTarif         :: ResponseDataTagihanTarif
  , rdtMinumLalu     :: Int64
  , rdtMinumSekarang :: Int64
  , rdtTanggalBayar  :: Maybe Day
  } deriving (Generic)
instance ToJSON ResponseDataTagihan where
  toJSON = genericToJSON omitsnake

data ResponseTagihanSimple = ResponseTagihanSimple
  { rtsTahun         :: Int64
  , rtsBulan         :: Int
  , rtsMinumLalu     :: Int64
  , rtsMinumSekarang :: Int64
  , rtsTanggalBayar  :: Maybe Day
  } deriving (Generic)
instance ToJSON ResponseTagihanSimple where
  toJSON = genericToJSON omitsnake

data ResponseRiwayatPelanggan = ResponseRiwayatPelanggan
  { rrpNamaPelanggan :: Text
  , rppNomorTelepon  :: Text
  , rppAlamat        :: Text
  , rppWilayah       :: Text
  , rppNomorMeteran  :: Text
  , rppTanggalDaftar :: Day
  , rppPenggunaanAir :: [ResponseTagihanSimple]
  } deriving (Generic)
instance ToJSON ResponseRiwayatPelanggan where
  toJSON = genericToJSON omitsnake

data ResponseIkhtisar = ResponseIkhtisar
  { riJumlahPelanggan :: Int64
  , riJumlahTagihan   :: Int64
  , riTagihanBayar    :: Int64
  , riTarif           :: ResponseDataTagihanTarif
  } deriving (Generic)
instance ToJSON ResponseIkhtisar where
  toJSON = genericToJSON omitsnake

data Gagal
  = GagalMasuk
  | GagalAdminNil Int64
  | GagalBayar Text
  | GagalCatatAir Text Text
  | GagalDB Text Text
  | GagalDataTidakAda
  | GagalMeteranAda Text
  | GagalMeteranNil Text
  | GagalMinumAda Text
  | GagalMinumNil Text
  | GagalPasswordBeda
  | GagalPenggunaAda Text
  | GagalPenggunaNil Text
  | GagalPenggunaTunaMeteran
  | GagalTagihanNil Text Int64 Int
  | GagalTagihanTahunBulanNil Integer Int
  | GagalTakBerwenang Text
  | GagalTambahPelanggan Text Text
  | GagalTanggalBelumAda Integer Int
  | GagalTanggalTidakValid Integer Int
  | GagalTanggalTidakAda
  | GagalTarifKosong
  | GagalUbahAir Text Text
  | GagalUpdatePassword Text
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
  { errBody = encodeRespError $ "nomor " <> (pack . show $ n) <> " bukan admin."
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
gagalToServantErr (GagalTagihanNil x y z) = err404
  { errBody = encodeRespError
    (  "pengguna dengan nomor "
    <> (pack . show $ x)
    <> " tidak punya tagihan di tahun "
    <> (pack . show $ y)
    <> " bulan "
    <> (pack . show $ z)
    <> "."
    )
  }
gagalToServantErr (GagalTagihanTahunBulanNil x y) = err404
  { errBody = encodeRespError
    (  "pelanggan tidak punya tagihan pada tahun "
    <> (pack . show $ x)
    <> " bulan "
    <> (pack . show $ y)
    <> "."
    )
  }
gagalToServantErr (GagalTakBerwenang saat) =
  err403 { errBody = encodeRespError saat }
gagalToServantErr (GagalTambahPelanggan a d) =
  err401 { errBody = encodeRespError (a <> "saat " <> d) }
gagalToServantErr GagalTanggalTidakAda =
  err400 { errBody = encodeRespError "harap isi tanggal." }
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

instance Exception Gagal
