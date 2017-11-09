{-# LANGUAGE OverloadedStrings #-}
module Request (Request, createToken, defaultRequest, getSpotList, getStampRally) where

import           Data.Aeson           (FromJSON, decode)
import qualified Data.ByteString.UTF8 as B
import           Data.Credential      (Credential)
import           Data.Maybe           (Maybe (Just))
import           Data.Monoid          ((<>))
import           Data.Spot            (SpotList)
import           Data.StampRally      (StampRally, StampRallyId)
import           Data.Token           (Token, getAuthHeader)
import           Network.HTTP.Simple  (Request, getResponseBody, httpLBS,
                                       parseRequest, setRequestBodyJSON,
                                       setRequestHeaders, setRequestMethod,
                                       setRequestPath, setRequestQueryString)
import           Prelude              (IO, pure, ($))

createToken :: Credential -> Request -> IO (Maybe Token)
createToken credential request =
  sendRequest $ createTokenRequest credential request

createTokenRequest :: Credential -> Request -> Request
createTokenRequest credential request =
  setRequestMethod "POST"
    $ setRequestBodyJSON credential
    $ setRequestPath "/tokens"
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request

defaultRequest :: IO Request
defaultRequest = parseRequest "https://api.rallyapp.jp"

getSpotList :: StampRallyId -> Token -> Request -> IO (Maybe SpotList)
getSpotList stampRallyId token request =
  sendRequest $ getSpotListRequest stampRallyId token request

getSpotListRequest :: StampRallyId -> Token -> Request -> Request
getSpotListRequest stampRallyId token request =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request
 where
  path       = B.fromString $ "/stamp_rallies/" <> stampRallyId <> "/spots"
  authHeader = B.fromString $ getAuthHeader token

getStampRally :: StampRallyId -> Token -> Request -> IO (Maybe StampRally)
getStampRally stampRallyId token request =
  sendRequest $ getStampRallyRequest stampRallyId token request

getStampRallyRequest :: StampRallyId -> Token -> Request -> Request
getStampRallyRequest stampRallyId token request =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request
 where
  path       = B.fromString $ "/stamp_rallies/" <> stampRallyId
  authHeader = B.fromString $ getAuthHeader token

sendRequest :: (FromJSON a) => Request -> IO (Maybe a)
sendRequest request = do
  json <- httpLBS request
  pure $ decode $ getResponseBody json
