{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    , getCommand
    ) where

import           Data.Foldable (find)
import           Data.Maybe    (fromMaybe, listToMaybe)
import           Prelude       (IO, String, putStrLn, ($), (.), (==))

getCommand :: [String] -> String
getCommand args = fromMaybe defaultCommand $ find (==input) commandList
 where
  defaultCommand = "help"
  commandList    = ["export", "import", defaultCommand]
  input          = fromMaybe "" . listToMaybe $ args

someFunc :: IO ()
someFunc = putStrLn "someFunc"
