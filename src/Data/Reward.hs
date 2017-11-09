{-# LANGUAGE OverloadedStrings #-}
module Data.Reward (Reward, RewardId, RewardList) where

import           Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import           Prelude    (Int, Show, ($), (<$>))

type RewardId = Int
data Reward = Reward RewardId deriving (Show)
data RewardList = RewardList [Reward] deriving (Show)

instance FromJSON Reward where
  parseJSON = withObject "Reward" $ \v -> Reward <$> v .: "id"

instance FromJSON RewardList where
  parseJSON = withObject "RewardList" $ \v -> RewardList <$> v .: "rewards"
