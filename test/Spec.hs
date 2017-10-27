{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude    (IO, Integer, return, ($), (+))
import           Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [
      "Test1" ~: (1 :: Integer) + 1 ~?= 2,
      "Test1" ~: (1 :: Integer) + 1 ~?= 2
    ]
  return ()
