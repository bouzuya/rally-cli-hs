{-# LANGUAGE OverloadedStrings #-}
module Data.StampRally (StampRally, StampRallyId) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object,
                             withObject, (.:), (.=))
import           Prelude    (Show, String, ($), (<$>))

type StampRallyId = String
newtype StampRally = StampRally StampRallyId deriving (Show)

instance FromJSON StampRally where
  parseJSON = withObject "StampRally" $ \v -> StampRally <$> v .: "id"

instance ToJSON StampRally where
  toJSON (StampRally id) =
    object [ "id" .= id
           ]
