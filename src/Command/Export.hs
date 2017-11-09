{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson.Text   (encodeToLazyText)
import           Data.Credential   (getCredential)
import           Data.Maybe        (maybe)
import           Data.StampRally   (StampRally, StampRallyId)
import           Data.Text.Lazy.IO (writeFile)
import           Data.Token        (Token)
import           Prelude           (FilePath, IO, print, pure)
import           Request           (Request, createToken, defaultRequest,
                                    getSpotList, getStampRally)
import           System.Directory  (createDirectory, doesDirectoryExist,
                                    getCurrentDirectory)
import           System.Exit       (die)
import           System.FilePath   ((</>))

ensureDirectory :: FilePath -> IO ()
ensureDirectory filePath = do
  exists <- doesDirectoryExist filePath
  if exists then pure () else createDirectory filePath

saveStampRally :: FilePath -> StampRally -> IO ()
saveStampRally directory stampRally = do
  let filePath = directory </> "stamp-rally.json"
  let content  = encodeToLazyText stampRally
  writeFile filePath content

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
  spotList  <- getSpotList stampRallyId token' baseRequest
  spotList' <- maybe (die "SpotList") pure spotList
  print spotList'
