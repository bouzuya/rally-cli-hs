{-# LANGUAGE OverloadedStrings #-}
module Data.Token (Token, getAuthHeader) where

import           Data.Aeson  (FromJSON (parseJSON), withObject, (.:))
import           Data.Monoid ((<>))
import           Prelude     (Show, String, ($), (<$>))

data Token = Token String deriving (Show)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token <$> v .: "token"

getAuthHeader :: Token -> String
getAuthHeader (Token token) = "Token token=\"" <> token <> "\""
