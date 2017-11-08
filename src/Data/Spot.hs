{-# LANGUAGE OverloadedStrings #-}
module Data.Spot (Spot, SpotId, SpotList) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object,
                             withObject, (.:), (.=))
import           Prelude    (Int, Show, String, ($), (<$>))

type SpotId = Int
data Spot = Spot SpotId deriving (Show)
data SpotList = SpotList [Spot] deriving (Show)

instance FromJSON Spot where
  parseJSON = withObject "Spot" $ \v -> Spot <$> v .: "id"

instance FromJSON SpotList where
  parseJSON = withObject "SpotList" $ \v -> SpotList <$> v .: "spots"
