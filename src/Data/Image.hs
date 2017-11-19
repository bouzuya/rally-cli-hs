{-# LANGUAGE OverloadedStrings #-}
module Data.Image
  ( Image
  , getUrl
  ) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object,
                             withObject, (.:), (.=))
import           Prelude    (Int, Show, String, ($), (<$>), (<*>))

data Image = Image String Int String String deriving (Show)

getUrl :: Image -> String
getUrl (Image _ _ _ s640) = s640

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v -> Image
    <$> v .: "id"
    <*> v .: "position"
    <*> v .: "s64"
    <*> v .: "s640"

instance ToJSON Image where
  toJSON (Image id position s64 s640) =
    object [ "id" .= id
           , "position" .= position
           , "s64" .= s64
           , "s640" .= s640
           ]
