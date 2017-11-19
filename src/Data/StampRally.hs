{-# LANGUAGE OverloadedStrings #-}
module Data.StampRally (StampRally, StampRallyId) where

import           Data.Aeson  (FromJSON (parseJSON), ToJSON (toJSON), object,
                              withObject, (.:), (.=))
import           Data.Detail (Detail)
import           Data.Image  (Image)
import           Prelude     (Show, String, ($), (<$>), (<*>))

type StampRallyId = String
data StampRally = StampRally StampRallyId [Detail] [Image] deriving (Show)

instance FromJSON StampRally where
  parseJSON = withObject "StampRally" $ \v -> StampRally
    <$> v .: "id"
    <*> v .: "details"
    <*> v .: "images"

instance ToJSON StampRally where
  toJSON (StampRally id details images) =
    object [ "id" .= id
           , "details" .= details
           , "images" .= images
           ]
