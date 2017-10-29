import           Lib        (getCommand)
import           Prelude    (IO, Integer, return, ($), (+))
import           Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [
      "getCommand" ~: getCommand [] ~?= "help",
      "getCommand" ~: getCommand [""] ~?= "help",
      "getCommand" ~: getCommand ["xxx"] ~?= "help",
      "getCommand" ~: getCommand ["export"] ~?= "export",
      "getCommand" ~: getCommand ["import"] ~?= "import",
      "getCommand" ~: getCommand ["help"] ~?= "help",
      "getCommand" ~: getCommand ["export", "help"] ~?= "export",
      "Test1" ~: (1 :: Integer) + 1 ~?= 2
    ]
  return ()
