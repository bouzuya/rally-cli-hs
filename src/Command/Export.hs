{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      decode, object, withObject, (.:), (.=))
import           Data.Maybe          (Maybe (Just, Nothing))
import           Network.HTTP.Simple (Request, getResponseBody, httpLBS,
                                      parseRequest, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath,
                                      setRequestQueryString)
import           Prelude             (IO, Show, String, print, pure, putStrLn,
                                      ($), (<$>), (<*>))
import           System.Environment  (lookupEnv)
import           System.Exit         (exitFailure)

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)
data Token = Token String deriving (Show)

getCredential :: IO (Maybe Credential)
getCredential = do
  email    <- lookupEnv "RALLY_EMAIL"
  password <- lookupEnv "RALLY_PASSWORD"
  pure $ Credential <$> email <*> password

instance ToJSON Credential where
  toJSON (Credential email password) =
    object [ "email" .= email
           , "password" .= password
           ]

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token <$> v .: "token"

createToken :: Credential -> Request -> IO (Maybe Token)
createToken credential baseRequest =
  sendRequest $ createTokenRequest credential baseRequest :: IO (Maybe Token)

createTokenRequest :: Credential -> Request -> Request
createTokenRequest credential baseRequest =
  setRequestMethod "POST"
    $ setRequestBodyJSON credential
    $ setRequestPath "/tokens"
    $ setRequestQueryString [("viewtype", Just "admin")]
    $ baseRequest

sendRequest :: (FromJSON a) => Request -> IO (Maybe a)
sendRequest request = do
  json <- httpLBS request
  pure $ decode $ getResponseBody json

export' :: IO ()
export' = do
  baseRequest <- parseRequest "https://api.rallyapp.jp"
  credential <- getCredential
  print credential
  token <- case credential of
    Just x -> createToken x baseRequest
    Nothing -> do
      putStrLn "check RALLY_EMAIL and RALLY_PASSWORD"
      exitFailure
  print token
