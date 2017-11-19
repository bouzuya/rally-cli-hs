{-# LANGUAGE OverloadedStrings #-}
module Request (Request
               , createToken
               , defaultRequest
               , getImage
               , getSpot
               , getSpotList
               , getRewardList
               , getStampRally
               ) where

import           Control.Monad        (foldM)
import           Data.Aeson           (FromJSON, decode)
import           Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B
import           Data.Credential      (Credential)
import           Data.Maybe           (Maybe (Just, Nothing))
import           Data.Monoid          ((<>))
import           Data.Reward          (Reward, RewardId, RewardList,
                                       getRewardId, getRewardSummaryList)
import           Data.Spot            (Spot, SpotId, SpotList, getSpotId,
                                       getSpotSummaryList)
import           Data.StampRally      (StampRally, StampRallyId)
import           Data.Token           (Token, getAuthHeader)
import           Network.HTTP.Simple  (Request, getResponseBody, httpLBS,
                                       parseRequest, setRequestBodyJSON,
                                       setRequestHeaders, setRequestMethod,
                                       setRequestPath, setRequestQueryString)
import           Prelude              (IO, String, id, pure, show, traverse,
                                       ($))

type LBS = L.ByteString

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

getImage :: String -> IO (Maybe LBS)
getImage url = do
  request <- parseRequest url
  response <- httpLBS request
  pure $ Just $ getResponseBody response -- TODO: check status code

getReward :: RewardId -> Token -> Request -> IO (Maybe Reward)
getReward rewardId token request =
  sendRequest $ getRewardRequest rewardId token request

getRewardRequest :: RewardId -> Token -> Request -> Request
getRewardRequest rewardId token request =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request
 where
  path       = B.fromString $ "/rewards/" <> show rewardId
  authHeader = B.fromString $ getAuthHeader token

getRewardList :: StampRallyId -> Token -> Request -> IO (Maybe [Reward])
getRewardList stampRallyId token request = do
  rewardSummaryList <- getRewardSummaryList' stampRallyId token request
  case rewardSummaryList of
    Just x -> do
      rewards <-
        foldM
          ( \a rewardSummary -> do
            reward <- getReward' rewardSummary
            pure $ a <> [reward]
          )
          []
          (getRewardSummaryList x) :: IO [Maybe Reward]
      pure $ traverse id rewards
    Nothing -> pure Nothing
 where
  getReward' rewardSummary =
    getReward (getRewardId rewardSummary) token request

getRewardSummaryList'
  :: StampRallyId -> Token -> Request -> IO (Maybe RewardList)
getRewardSummaryList' stampRallyId token request =
  sendRequest $ getRewardSummaryListRequest stampRallyId token request

getRewardSummaryListRequest :: StampRallyId -> Token -> Request -> Request
getRewardSummaryListRequest stampRallyId token request =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request
 where
  path       = B.fromString $ "/stamp_rallies/" <> stampRallyId <> "/rewards"
  authHeader = B.fromString $ getAuthHeader token

getSpot :: SpotId -> Token -> Request -> IO (Maybe Spot)
getSpot spotId token request =
  sendRequest $ getSpotRequest spotId token request

getSpotRequest :: SpotId -> Token -> Request -> Request
getSpotRequest spotId token request =
  setRequestMethod "GET"
    $ setRequestPath path
    $ setRequestHeaders [("Authorization", authHeader)]
    $ setRequestQueryString [("view_type", Just "admin")]
    $ request
 where
  path       = B.fromString $ "/spots/" <> show spotId
  authHeader = B.fromString $ getAuthHeader token

getSpotList :: StampRallyId -> Token -> Request -> IO (Maybe [Spot])
getSpotList stampRallyId token request = do
  spotSummaryList <- getSpotSummaryList' stampRallyId token request
  case spotSummaryList of
    Just x -> do
      spots <-
        foldM
          ( \a spotSummary -> do
            spot <- getSpot' spotSummary
            pure $ a <> [spot]
          )
          []
          (getSpotSummaryList x) :: IO [Maybe Spot]
      pure $ traverse id spots
    Nothing -> pure Nothing
  where getSpot' spotSummary = getSpot (getSpotId spotSummary) token request

getSpotSummaryList' :: StampRallyId -> Token -> Request -> IO (Maybe SpotList)
getSpotSummaryList' stampRallyId token request =
  sendRequest $ getSpotSummaryListRequest stampRallyId token request

getSpotSummaryListRequest :: StampRallyId -> Token -> Request -> Request
getSpotSummaryListRequest stampRallyId token request =
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
  response <- httpLBS request
  pure $ decode $ getResponseBody response
