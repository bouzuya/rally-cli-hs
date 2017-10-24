{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    ) where

import           Prelude (IO, putStrLn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
