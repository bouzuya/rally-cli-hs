{-# LANGUAGE NoImplicitPrelude #-}

import           Prelude    (IO, Integer, return, ($), (+))
import           Test.HUnit (runTestTT, (~:), (~?=))

main :: IO ()
main = do
  _ <- runTestTT $ "Test1" ~: (1 :: Integer) + 1 ~?= 2
  return ()
