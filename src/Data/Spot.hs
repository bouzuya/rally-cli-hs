{-# LANGUAGE OverloadedStrings #-}
module Data.Spot ( Spot
                 , SpotId
                 , SpotList
                 , SpotSummary
                 , getSpotId
                 , getSpotSummaryList
                 ) where

import           Data.Aeson  (FromJSON (parseJSON), ToJSON (toJSON), object,
                              withObject, (.:), (.=))
import           Data.Detail (Detail)
import           Prelude     (Int, Show, ($), (<$>), (<*>))

type SpotId = Int
data Spot = Spot SpotId [Detail] deriving (Show)
data SpotSummary = SpotSummary SpotId deriving (Show)
data SpotList = SpotList [SpotSummary] deriving (Show)

getSpotId :: SpotSummary -> SpotId
getSpotId (SpotSummary spotId) = spotId

getSpotSummaryList :: SpotList -> [SpotSummary]
getSpotSummaryList (SpotList spotSummaryList) = spotSummaryList

instance FromJSON Spot where
  parseJSON = withObject "Spot" $ \v -> Spot
    <$> v .: "id"
    <*> v .: "details"

instance FromJSON SpotSummary where
  parseJSON = withObject "SpotSummary" $ \v -> SpotSummary <$> v .: "id"

instance FromJSON SpotList where
  parseJSON = withObject "SpotList" $ \v -> SpotList
    <$> v .: "spots"

instance ToJSON Spot where
  toJSON (Spot id details) =
    object [ "id" .= id
           , "details" .= details
           ]
