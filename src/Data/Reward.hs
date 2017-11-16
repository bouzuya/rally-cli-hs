{-# LANGUAGE OverloadedStrings #-}
module Data.Reward ( Reward
                   , RewardId
                   , RewardList
                   , getRewardId
                   , getRewardSummaryList
                   ) where

import           Data.Aeson  (FromJSON (parseJSON), withObject, (.:))
import           Data.Detail (Detail)
import           Prelude     (Int, Show, ($), (<$>), (<*>))

type RewardId = Int
data Reward = Reward RewardId [Detail] deriving (Show)
data RewardSummary = RewardSummary RewardId deriving (Show)
data RewardList = RewardList [RewardSummary] deriving (Show)

getRewardId :: RewardSummary -> RewardId
getRewardId (RewardSummary rewardId) = rewardId

getRewardSummaryList :: RewardList -> [RewardSummary]
getRewardSummaryList (RewardList rewardSummaryList) = rewardSummaryList

instance FromJSON Reward where
  parseJSON = withObject "Reward" $ \v -> Reward
    <$> v .: "id"
    <*> v .: "details"

instance FromJSON RewardSummary where
  parseJSON = withObject "RewardSummary" $ \v -> RewardSummary
    <$> v .: "id"

instance FromJSON RewardList where
  parseJSON = withObject "RewardList" $ \v -> RewardList
    <$> v .: "rewards"

