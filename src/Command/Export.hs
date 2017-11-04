module Command.Export (export') where

import           Data.Maybe         (Maybe)
import           Prelude            (IO, Show, String, print, pure, return, ($),
                                     (<$>), (<*>))
import           System.Environment (lookupEnv)

type Email = String
type Password = String
data Credential = Credential Email Password deriving (Show)

getCredential :: IO (Maybe Credential)
getCredential = do
  email <- lookupEnv "RALLY_EMAIL"
  password <- lookupEnv "RALLY_PASSWORD"
  pure $ Credential <$> email <*> password

export' :: IO ()
export' = do
  credential <- getCredential
  print credential
  return ()
