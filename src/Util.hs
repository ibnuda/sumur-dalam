{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Util where

import           Protolude

import           Crypto.BCrypt
import           Crypto.JOSE
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString       as B
import           Data.Text             (unpack)
import           Data.Time.Clock
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

buatPassword :: Text -> IO Text
buatPassword password = do
  mpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ BC.pack $ unpack
    password
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

-- | "https://github.com/haskell-servant/servant-auth/pull/107/commits/3813a4d979dfbd47b6f9b667dfe163dd4743c141"
generateSecret :: MonadRandom m => m ByteString
generateSecret = getRandomBytes 256

fromSecret :: ByteString -> JWK
fromSecret = fromOctets

writeKey :: FilePath -> IO ()
writeKey filepath = B.writeFile filepath =<< generateSecret

readKey :: FilePath -> IO JWK
readKey filepath = fromSecret <$> B.readFile filepath
