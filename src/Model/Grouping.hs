{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Grouping where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Casing
import           Database.Persist.TH

data GrupSistem
  = SuperAdmin
  | Admin
  | Petugas
  | Pelanggan
  deriving (Show, Eq, Read, Ord, Generic)

instance ToJSON GrupSistem where
  toJSON = genericToJSON omitsnake
    where
      omitsnake :: Options
      omitsnake =
        let snake = aesonPrefix snakeCase
         in snake {omitNothingFields = True}

derivePersistField "GrupSistem"
