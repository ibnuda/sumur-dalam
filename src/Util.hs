{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Util where

import           Protolude

import           Crypto.BCrypt
import           Crypto.JOSE

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Time

import           Database.Esqueleto

import           Servant.Auth.Server

import           Conf
import           Model
import           Types

buatPassword :: Text -> IO Text
buatPassword password = do
  mpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy
    $ encodeUtf8 password
  case mpass of
    Nothing -> buatPassword password
    Just pa -> return $ decodeUtf8 pa

buatToken
  :: (MonadError Gagal m, MonadReader Konfigurasi m, MonadIO m)
  => Pengguna
  -> m Text
buatToken pengguna = do
  now    <- liftIO getCurrentTime
  jws    <- asks konfigurasiSettingJWT
  etoken <- liftIO $ makeJWT pengguna jws (Just $ addUTCTime nominalDay now)
  case etoken of
    Left  _ -> throwError GagalMasuk
    Right y -> return . decodeUtf8 . BL.toStrict $ y

untuple :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
untuple f (a, b) = f a b

untriple :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
untriple f (a, b, c) = f a b c

unquadruple :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
unquadruple f (a, b, c, d) = f a b c d

unquintuple :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6) -> (t1, t2, t3, t4, t5) -> t6
unquintuple f (a, b, c, d, e) = f a b c d e

unsextuple
  :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7) -> (t1, t2, t3, t4, t5, t6) -> t7
unsextuple f (a, b, c, d, e, f') = f a b c d e f'

tarifKeResponse :: Tarif -> ResponseDataTagihanTarif
tarifKeResponse Tarif {..} = ResponseDataTagihanTarif tarifHargaAwal
                                                      tarifSampaiAwal
                                                      tarifHargaTengah
                                                      tarifSampaiTengah
                                                      tarifHargaAkhir
                                                      tarifBiayaBeban

penggunaMeteranKeResponse
  :: Entity Pengguna -> Entity Meteran -> ResponseDataTagihanPengguna
penggunaMeteranKeResponse (Entity pid Pengguna {..}) (Entity _ Meteran {..}) =
  ResponseDataTagihanPengguna (fromSqlKey pid)
                              penggunaNama
                              penggunaNomorTelp
                              penggunaAlamat
                              penggunaWilayah

querytagihanKeResponse
  :: Entity Pengguna
  -> Entity Meteran
  -> Entity Tagihan
  -> Entity Minum
  -> Entity Tarif
  -> Value Int64
  -> ResponseDataTagihan
querytagihanKeResponse pengguna meteran tagihan minum tarif (Value lalu) =
  ResponseDataTagihan (fromSqlKey $ entityKey tagihan)
                      (meteranNomor $ entityVal meteran)
                      (minumTahun $ entityVal minum)
                      (minumBulan $ entityVal minum)
                      (penggunaMeteranKeResponse pengguna meteran)
                      (tarifKeResponse $ entityVal tarif)
                      lalu
                      (minumSampai $ entityVal minum)
                      (tagihanTanggalBayar $ entityVal tagihan)

querytagihanpenggunaKeResponse
  :: [ ( Entity Pengguna
       , Entity Meteran
       , Entity Tagihan
       , Entity Minum
       , Entity Tarif
       )
     ]
  -> [ResponseDataTagihan]
querytagihanpenggunaKeResponse [] = []
querytagihanpenggunaKeResponse [(p, m, t, mi, ta)] =
  [querytagihanKeResponse p m t mi ta (Value 0)]
querytagihanpenggunaKeResponse ((p, m, t, mi, ta) : (p', m', t', mi', ta') : xs)
  = let lalu = minumSampai $ entityVal mi'
    in  querytagihanKeResponse p m t mi ta (Value lalu)
          : querytagihanpenggunaKeResponse ((p', m', t', mi', ta') : xs)

queryriwayatKeResponse
  :: Entity Pengguna
  -> Entity Meteran
  -> [(Entity Minum, Value (Maybe Day))]
  -> ResponseRiwayatPelanggan
queryriwayatKeResponse p m ts =
  let Pengguna {..} = entityVal p
      Meteran {..}  = entityVal m
  in  ResponseRiwayatPelanggan penggunaNama
                               penggunaNomorTelp
                               penggunaAlamat
                               penggunaWilayah
                               meteranNomor
                               meteranTanggalDaftar
                               (querytagihansimpleKeResponse ts)

querytagihansimpleKeResponse
  :: [(Entity Minum, Value (Maybe Day))] -> [ResponseTagihanSimple]
querytagihansimpleKeResponse [] = []
querytagihansimpleKeResponse [(m, tag)] =
  let Minum {..}   = entityVal m
      tanggalbayar = unValue tag
  in  [ResponseTagihanSimple minumTahun minumBulan 0 minumSampai tanggalbayar]
querytagihansimpleKeResponse ((m, tag) : x@(m', _) : xs) =
  let minum        = entityVal m
      tanggalbayar = unValue tag
      minumlalu    = minumSampai (entityVal m')
  in  (ResponseTagihanSimple (minumTahun minum)
                             (minumBulan minum)
                             minumlalu
                             (minumSampai minum)
                             tanggalbayar
      )
        : querytagihansimpleKeResponse (x : xs)

-- | "https://github.com/haskell-servant/servant-auth/pull/107/commits/3813a4d979dfbd47b6f9b667dfe163dd4743c141"
generateSecret :: MonadRandom m => m ByteString
generateSecret = getRandomBytes 256

fromSecret :: ByteString -> JWK
fromSecret = fromOctets

writeKey :: FilePath -> IO ()
writeKey filepath = B.writeFile filepath =<< generateSecret

readKey :: FilePath -> IO JWK
readKey filepath = fromSecret <$> B.readFile filepath
