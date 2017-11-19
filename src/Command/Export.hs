{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson           (ToJSON, encode)
import           Data.ByteString.Lazy as L
import           Data.Credential      (getCredential)
import qualified Data.Image           as Image
import           Data.Maybe           (Maybe (Just, Nothing), listToMaybe,
                                       maybe)
import           Data.Reward          (Reward)
import           Data.Spot            (Spot)
import           Data.StampRally      (StampRally, StampRallyId)
import qualified Data.StampRally      as StampRally
import           Data.Token           (Token)
import           Prelude              (FilePath, IO, String, pure, ($), (<$>))
import           Request              (Request, createToken, defaultRequest,
                                       getImage, getRewardList, getSpotList,
                                       getStampRally)
import           System.Directory     (createDirectory, doesDirectoryExist,
                                       getCurrentDirectory)
import           System.Exit          (die)
import           System.FilePath      ((</>))

type LBS = L.ByteString
type Get a = StampRallyId -> Token -> Request -> IO (Maybe a)
type Save a = FilePath -> a -> IO ()

ensureDirectory :: FilePath -> IO ()
ensureDirectory filePath = do
  exists <- doesDirectoryExist filePath
  if exists then pure () else createDirectory filePath

saveBinary :: FilePath -> FilePath -> LBS -> IO ()
saveBinary fileName directory content = do
  let filePath = directory </> fileName
  L.writeFile filePath content

save :: (ToJSON a) => FilePath -> Save a
save fileName directory o = do
  let filePath = directory </> fileName
  let content  = encode o
  L.writeFile filePath content

saveRewardList :: Save [Reward]
saveRewardList = save "rewards.json"

saveSpotList :: Save [Spot]
saveSpotList = save "spots.json"

saveStampRally :: Save StampRally
saveStampRally = save "stamp-rally.json"

export
  :: (ToJSON a)
  => String
  -> Get a
  -> Save a
  -> FilePath
  -> StampRallyId
  -> Token
  -> Request
  -> IO ()
export message get save' directory stampRallyId token request = do
  x <- get stampRallyId token request
  maybe (die message) (save' directory) x

exportRewardList :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportRewardList = export "RewardList" getRewardList saveRewardList

exportSpotList :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportSpotList = export "SpotList" getSpotList saveSpotList

exportStampRally :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportStampRally = export "StampRally" getStampRally saveStampRally

exportStampRallyImages :: FilePath -> StampRallyId -> Token -> Request -> IO ()
exportStampRallyImages stampRallyDirectory stampRallyId token' baseRequest = do
  stampRally  <- getStampRally stampRallyId token' baseRequest
  stampRally' <- maybe (die "StampRally (Image)") pure stampRally
  let imageUrl =
        Image.getUrl <$> (listToMaybe $ StampRally.getImageList stampRally')
  case imageUrl of
    Just u -> do
      imageData  <- getImage u
      imageData' <- maybe (die "StampRally (ImageData)") pure imageData
      saveBinary "s640.png" stampRallyDirectory imageData'
    Nothing -> pure ()

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
  exportStampRally       stampRallyDirectory stampRallyId token' baseRequest
  exportSpotList         stampRallyDirectory stampRallyId token' baseRequest
  exportRewardList       stampRallyDirectory stampRallyId token' baseRequest
  exportStampRallyImages stampRallyDirectory stampRallyId token' baseRequest
