{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson           (FromJSON (parseJSON), ToJSON (toJSON),
                                       decode, object, withObject, (.:), (.=))
import qualified Data.ByteString.UTF8 as B
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.Monoid          ((<>))
import           Network.HTTP.Simple  (Request, getResponseBody, httpLBS,
                                       parseRequest, setRequestBodyJSON,
                                       setRequestHeaders, setRequestMethod,
                                       setRequestPath, setRequestQueryString)
import           Prelude              (IO, Show, String, print, pure, putStrLn,
                                       ($), (<$>), (<*>))
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure)

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)
type StampRallyId = String
data StampRally = StampRally StampRallyId deriving (Show)
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

instance FromJSON StampRally where
  parseJSON = withObject "StampRally" $ \v -> StampRally <$> v .: "id"

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token <$> v .: "token"

createToken :: Request -> IO (Maybe Token)
createToken = sendRequest

createTokenRequest :: Credential -> Request -> Request
createTokenRequest credential baseRequest =
  setRequestMethod "POST"
    $ setRequestBodyJSON credential
    $ setRequestPath "/tokens"
    $ setRequestQueryString [("view_type", Just "admin")]
    $ baseRequest

getStampRally :: Request -> IO (Maybe StampRally)
getStampRally = sendRequest

getStampRallyRequest :: StampRallyId -> Token -> Request -> Request
getStampRallyRequest stampRallyId (Token token) baseRequest =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ baseRequest
  where
    path = B.fromString $ "/stamp_rallies/" <> stampRallyId
    authHeader = B.fromString $ "Token token=\"" <> token <> "\""

sendRequest :: (FromJSON a) => Request -> IO (Maybe a)
sendRequest request = do
  json <- httpLBS request
  pure $ decode $ getResponseBody json

export' :: IO ()
export' = do
  let stampRallyId = "xxxxxxxxxxxxxxxx"
  baseRequest <- parseRequest "https://api.rallyapp.jp"
  credential <- getCredential
  print credential
  token <- case credential of
    Just x -> createToken $ createTokenRequest x baseRequest
    Nothing -> do
      putStrLn "check RALLY_EMAIL and RALLY_PASSWORD"
      exitFailure
  print token
  stampRally <- case token of
    Just x -> getStampRally $ getStampRallyRequest stampRallyId x baseRequest
    Nothing -> do
      putStrLn "StampRally"
      exitFailure
  print stampRally
