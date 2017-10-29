module Main (main) where

import           Lib                (getCommand, someFunc)
import           Prelude            (IO, putStrLn, (<$>))
import           System.Environment (getArgs)

main :: IO ()
main = do
  command <- getCommand <$> getArgs
  putStrLn command
  someFunc
