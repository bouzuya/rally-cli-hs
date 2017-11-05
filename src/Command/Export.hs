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
import           Prelude              (FilePath, IO, Int, Show, String, print,
                                       pure, putStrLn, ($), (<$>), (<*>))
import           System.Directory     (createDirectory, doesDirectoryExist,
                                       getCurrentDirectory)
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure)
import           System.FilePath      ((</>))

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)
type StampRallyId = String
data StampRally = StampRally StampRallyId deriving (Show)
data Token = Token String deriving (Show)
type SpotId = Int
data Spot = Spot SpotId deriving (Show)
data SpotList = SpotList [Spot] deriving (Show)

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

instance FromJSON Spot where
  parseJSON = withObject "Spot" $ \v -> Spot <$> v .: "id"

instance FromJSON SpotList where
  parseJSON = withObject "SpotList" $ \v -> SpotList <$> v .: "spots"

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

getSpotList :: Request -> IO (Maybe SpotList)
getSpotList = sendRequest

getSpotListRequest :: StampRallyId -> Token -> Request -> Request
getSpotListRequest stampRallyId (Token token) baseRequest =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ baseRequest
  where
    path = B.fromString $ "/stamp_rallies/" <> stampRallyId <> "/spots"
    authHeader = B.fromString $ "Token token=\"" <> token <> "\""

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

ensureDirectory :: FilePath -> IO ()
ensureDirectory filePath = do
  exists <- doesDirectoryExist filePath
  if exists then pure () else createDirectory filePath

export' :: IO ()
export' = do
  let stampRallyId = "xxxxxxxxxxxxxxxx"
  currentDirectory <- getCurrentDirectory
  putStrLn currentDirectory
  let stampRallyDirectory = currentDirectory </> stampRallyId
  putStrLn stampRallyDirectory
  ensureDirectory stampRallyDirectory
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
  spotList <- case token of
    Just x -> getSpotList $ getSpotListRequest stampRallyId x baseRequest
    Nothing -> do
      putStrLn "SpotList"
      exitFailure
  print spotList
