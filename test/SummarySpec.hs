module SummarySpec (tests) where

import Data.Time (fromGregorian)
import Summary (WeeklyEpisode (..), toWeeklyEpisodes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TheMovieDb (Episode (..), Series (..))
import Data.Time.Calendar (Day)
import Data.Text (Text)

tests :: TestTree
tests = testGroup "Summary" [toWeeklyEpisodesTests]

toWeeklyEpisodesTests :: TestTree
toWeeklyEpisodesTests =
    testGroup
        "toWeeklyEpisodes"
        [ testCase "no last or next episode" $
            toWeeklyEpisodes (mkSeries "X" Nothing Nothing) @?= []
        , testCase "only next" $
            let aired = fromGregorian 2022 5 4
                ep = mkEpisode aired 1 1
             in toWeeklyEpisodes (mkSeries "X" Nothing (Just ep)) @?= [WeeklyEpisode "X" aired 1 1]
        , testCase "only previous" $
            let aired = fromGregorian 2022 5 4
                ep = mkEpisode aired 1 2
             in toWeeklyEpisodes (mkSeries "X" (Just ep) Nothing) @?= [WeeklyEpisode "X" aired 1 2]
        , testCase "both" $
            let aired1 = fromGregorian 2022 5 4
                ep1 = mkEpisode aired1 1 2
                aired2 = fromGregorian 2022 5 6
                ep2 = mkEpisode aired2 1 3
             in toWeeklyEpisodes (mkSeries "X" (Just ep1) (Just ep2))
                    @?= [WeeklyEpisode "X" aired1 1 2, WeeklyEpisode "X" aired2 1 3]
        ]

mkSeries :: Text -> Maybe Episode -> Maybe Episode -> Series
mkSeries name lst next =
    Series
        { sId = 0
        , sName = name
        , sLastEpisodeToAir = lst
        , sNextEpisodeToAir = next
        }

mkEpisode :: Day -> Integer -> Integer -> Episode
mkEpisode aired seasonNum epNum =
    Episode
        { eAirDate = aired
        , eEpisodeNumber = epNum
        , eId = 0
        , eName = ""
        , eOverview = ""
        , eSeasonNumber = seasonNum
        , eRuntime = Nothing
        }