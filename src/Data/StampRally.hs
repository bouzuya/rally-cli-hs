{-# LANGUAGE OverloadedStrings #-}
module Data.StampRally (StampRally, StampRallyId) where

import           Data.Aeson  (FromJSON (parseJSON), ToJSON (toJSON), object,
                              withObject, (.:), (.=))
import           Data.Detail (Detail)
import           Prelude     (Show, String, ($), (<$>), (<*>))

type StampRallyId = String
data StampRally = StampRally StampRallyId [Detail] deriving (Show)

instance FromJSON StampRally where
  parseJSON = withObject "StampRally" $ \v -> StampRally
    <$> v .: "id"
    <*> v .: "details"

instance ToJSON StampRally where
  toJSON (StampRally id details) =
    object [ "id" .= id
           , "details" .= details
           ]
