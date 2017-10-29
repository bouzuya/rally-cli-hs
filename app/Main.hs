{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import           Data.Foldable      (find)
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Lib                (someFunc)
import           Prelude            (IO, String, putStrLn, ($), (.), (<$>),
                                     (==))
import           System.Environment (getArgs)

getCommand :: [String] -> String
getCommand args = fromMaybe defaultCommand $ find (==input) commandList
 where
  defaultCommand = "help"
  commandList    = ["export", "import", defaultCommand]
  input          = fromMaybe "" . listToMaybe $ args

main :: IO ()
main = do
  command <- getCommand <$> getArgs
  putStrLn command
  someFunc
