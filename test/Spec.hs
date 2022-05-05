import qualified ScheduleSpec
import qualified SummarySpec
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [ScheduleSpec.tests, SummarySpec.tests]