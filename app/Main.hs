{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import           Data.List          (intercalate)
import           Lib                (someFunc)
import           Prelude            (IO, putStrLn, ($))
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ intercalate "," args
  someFunc
