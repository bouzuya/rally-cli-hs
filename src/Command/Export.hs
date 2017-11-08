{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson           (FromJSON, decode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.UTF8 as B
import           Data.Credential      (Credential, getCredential)
import           Data.Maybe           (Maybe (Just), maybe)
import           Data.Monoid          ((<>))
import           Data.Spot            (SpotList)
import           Data.StampRally      (StampRally, StampRallyId)
import           Data.Text.Lazy       as TL
import           Data.Token           (Token, getAuthHeader)
import           Network.HTTP.Simple  (Request, getResponseBody, httpLBS,
                                       parseRequest, setRequestBodyJSON,
                                       setRequestHeaders, setRequestMethod,
                                       setRequestPath, setRequestQueryString)
import           Prelude              (FilePath, IO, print, pure, ($))
import           System.Directory     (createDirectory, doesDirectoryExist,
                                       getCurrentDirectory)
import           System.Exit          (die)
import           System.FilePath      ((</>))
import           System.IO            (writeFile)

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
getSpotListRequest stampRallyId token baseRequest =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ baseRequest
 where
  path       = B.fromString $ "/stamp_rallies/" <> stampRallyId <> "/spots"
  authHeader = B.fromString $ getAuthHeader token

getStampRally :: Request -> IO (Maybe StampRally)
getStampRally = sendRequest

getStampRallyRequest :: StampRallyId -> Token -> Request -> Request
getStampRallyRequest stampRallyId token baseRequest =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ baseRequest
 where
  path       = B.fromString $ "/stamp_rallies/" <> stampRallyId
  authHeader = B.fromString $ getAuthHeader token

sendRequest :: (FromJSON a) => Request -> IO (Maybe a)
sendRequest request = do
  json <- httpLBS request
  pure $ decode $ getResponseBody json

ensureDirectory :: FilePath -> IO ()
ensureDirectory filePath = do
  exists <- doesDirectoryExist filePath
  if exists then pure () else createDirectory filePath

saveStampRally :: FilePath -> StampRally -> IO ()
saveStampRally directory stampRally = do
  let filePath = directory </> "stamp-rally.json"
  let content  = TL.unpack $ encodeToLazyText stampRally
  writeFile filePath content

exportStampRally :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportStampRally directory stampRallyId token request = do
  stampRally <- getStampRally $ getStampRallyRequest stampRallyId token request
  maybe (die "StampRally") (saveStampRally directory) stampRally

export' :: IO ()
export' = do
  let stampRallyId = "xxxxxxxxxxxxxxxx"
  currentDirectory <- getCurrentDirectory
  let stampRallyDirectory = currentDirectory </> stampRallyId
  ensureDirectory stampRallyDirectory
  baseRequest <- parseRequest "https://api.rallyapp.jp"
  credential  <- getCredential
  credential' <- maybe (die "RALLY_EMAIL & RALLY_PASSWORD") pure credential
  token       <- createToken $ createTokenRequest credential' baseRequest
  token'      <- maybe (die "Token") pure token
  exportStampRally stampRallyDirectory stampRallyId token' baseRequest
  spotList  <- getSpotList $ getSpotListRequest stampRallyId token' baseRequest
  spotList' <- maybe (die "SpotList") pure spotList
  print spotList'
