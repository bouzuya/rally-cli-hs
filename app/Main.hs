module Main (main) where

import           Command.Export     (export')
import           Lib                (getCommand, someFunc)
import           Prelude            (IO, putStrLn, (<$>), (==))
import           System.Environment (getArgs)

main :: IO ()
main = do
  command <- getCommand <$> getArgs
  if command == "export" then export' else putStrLn command
  someFunc
