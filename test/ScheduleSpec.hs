module ScheduleSpec (tests) where

import Data.Time (DayOfWeek (Saturday, Sunday), dayOfWeek)
import Data.Time.Calendar (fromGregorian)
import Schedule (getWeekContaining)
import Test.QuickCheck.Instances.Time ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Schedule" [getWeekContainingTests]

getWeekContainingTests :: TestTree
getWeekContainingTests =
    testGroup
        "getWeekContaining"
        [ QC.testProperty "starts on Sunday" $
            \day -> dayOfWeek (fst (getWeekContaining day)) == Sunday
        , QC.testProperty "ends on Saturday" $
            \day -> dayOfWeek (snd (getWeekContaining day)) == Saturday
        , QC.testProperty "start is before end" $
            \day -> let (first, second) = getWeekContaining day in first < second
        , QC.testProperty "contains requested date" $
            \day -> let (first, second) = getWeekContaining day in day <= second && day >= first
        , testCase "Sunday gets the week that starts with it" $
            let (first, _) = getWeekContaining (fromGregorian 2022 5 1)
             in first @?= fromGregorian 2022 5 1
        , testCase "Sunday gets the week that ends after it" $
            let (_, second) = getWeekContaining (fromGregorian 2022 5 1)
             in second @?= fromGregorian 2022 5 7
        , testCase "Saturday gets the week that starts before it" $
            let (first, _) = getWeekContaining (fromGregorian 2022 5 7)
             in first @?= fromGregorian 2022 5 1
        , testCase "Saturday gets the week that ends with it" $
            let (_, second) = getWeekContaining (fromGregorian 2022 5 7)
             in second @?= fromGregorian 2022 5 7
        ]