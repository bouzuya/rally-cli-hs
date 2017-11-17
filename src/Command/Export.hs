{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson        (ToJSON)
import           Data.Aeson.Text   (encodeToLazyText)
import           Data.Credential   (getCredential)
import           Data.Maybe        (maybe)
import           Data.Spot         (Spot)
import           Data.StampRally   (StampRally, StampRallyId)
import           Data.Text.Lazy.IO (writeFile)
import           Data.Token        (Token)
import           Prelude           (FilePath, IO, print, pure)
import           Request           (Request, createToken, defaultRequest,
                                    getRewardList, getSpotList, getStampRally)
import           System.Directory  (createDirectory, doesDirectoryExist,
                                    getCurrentDirectory)
import           System.Exit       (die)
import           System.FilePath   ((</>))

ensureDirectory :: FilePath -> IO ()
ensureDirectory filePath = do
  exists <- doesDirectoryExist filePath
  if exists then pure () else createDirectory filePath

save :: (ToJSON a) => FilePath -> FilePath -> a -> IO ()
save fileName directory o = do
  let filePath = directory </> fileName
  let content  = encodeToLazyText o
  writeFile filePath content

saveSpotList :: FilePath -> [Spot] -> IO ()
saveSpotList = save "spots.json"

saveStampRally :: FilePath -> StampRally -> IO ()
saveStampRally = save "stamp-rally.json"

exportSpotList :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportSpotList directory stampRallyId token request = do
  spotList <- getSpotList stampRallyId token request
  maybe (die "SpotList") (saveSpotList directory) spotList

exportStampRally :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportStampRally directory stampRallyId token request = do
  stampRally <- getStampRally stampRallyId token request
  maybe (die "StampRally") (saveStampRally directory) stampRally

export' :: IO ()
export' = do
  let stampRallyId = "xxxxxxxxxxxxxxxx"
  currentDirectory <- getCurrentDirectory
  let stampRallyDirectory = currentDirectory </> stampRallyId
  ensureDirectory stampRallyDirectory
  credential  <- getCredential
  credential' <- maybe (die "RALLY_EMAIL & RALLY_PASSWORD") pure credential
  baseRequest <- defaultRequest
  token       <- createToken credential' baseRequest
  token'      <- maybe (die "Token") pure token
  exportStampRally stampRallyDirectory stampRallyId token' baseRequest
  exportSpotList   stampRallyDirectory stampRallyId token' baseRequest
  rewardList  <- getRewardList stampRallyId token' baseRequest
  rewardList' <- maybe (die "RewardList") pure rewardList
  print rewardList'
