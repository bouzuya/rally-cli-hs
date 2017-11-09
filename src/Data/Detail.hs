{-# LANGUAGE OverloadedStrings #-}
module Data.Detail (Detail) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object,
                             withObject, (.:), (.=))
import           Prelude    (Show, String, ($), (<$>), (<*>))

data Detail = Detail String String deriving (Show)

instance FromJSON Detail where
  parseJSON = withObject "Detail" $ \v -> Detail <$> v .: "name" <*> v .: "value"

instance ToJSON Detail where
  toJSON (Detail name value) =
    object [ "name" .= name
           , "value" .= value
           ]
