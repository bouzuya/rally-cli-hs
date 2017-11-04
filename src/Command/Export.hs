{-# LANGUAGE OverloadedStrings #-}
module Command.Export (export') where

import           Data.Aeson          (ToJSON (toJSON), object, (.=))
import           Data.Maybe          (Maybe (Just, Nothing))
import           Network.HTTP.Simple (Request, getResponseBody, httpLBS,
                                      parseRequest, setRequestBodyJSON,
                                      setRequestMethod, setRequestPath,
                                      setRequestQueryString)
import           Prelude             (IO, Show, String, print, pure, putStrLn,
                                      ($), (<$>), (<*>))
import           System.Environment  (lookupEnv)

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)

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

createTokenRequest :: Credential -> Request -> Request
createTokenRequest credential baseRequest =
  setRequestMethod "POST"
    $ setRequestBodyJSON credential
    $ setRequestPath "/tokens"
    $ setRequestQueryString [("viewtype", Just "admin")]
    $ baseRequest

export' :: IO ()
export' = do
  baseRequest <- parseRequest "https://api.rallyapp.jp"
  credential <- getCredential
  print credential
  case credential of
    Just x -> do
      let request = createTokenRequest x baseRequest
      json <- httpLBS request
      print $ getResponseBody json
    Nothing ->
      putStrLn "check RALLY_EMAIL and RALLY_PASSWORD"
