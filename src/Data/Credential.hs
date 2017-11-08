{-# LANGUAGE OverloadedStrings #-}
module Data.Credential (Credential, getCredential) where

import           Data.Aeson         (ToJSON (toJSON), object, (.=))
import           Data.Maybe         (Maybe)
import           Prelude            (IO, Show, String, pure, ($), (<$>), (<*>))
import           System.Environment (lookupEnv)

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)

instance ToJSON Credential where
  toJSON (Credential email password) =
    object [ "email" .= email
           , "password" .= password
           ]

getCredential :: IO (Maybe Credential)
getCredential = do
  email    <- lookupEnv "RALLY_EMAIL"
  password <- lookupEnv "RALLY_PASSWORD"
  pure $ Credential <$> email <*> password
