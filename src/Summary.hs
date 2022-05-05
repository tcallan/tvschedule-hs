module Summary (
    toThisWeek,
    getSummary,
    toWeeklyEpisodes,
    WeeklyEpisode (..),
    SeriesSummary (..),
    Summary (..),
    tshow,
    tshowOpt,
    toEpNumber,
) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map, fromListWith)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Time (Day)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Schedule (getWeekContaining, today)
import TheMovieDb (
    Episode (eAirDate, eEpisodeNumber, eSeasonNumber),
    Series (sLastEpisodeToAir, sName, sNextEpisodeToAir),
    serieses,
 )

data Summary = Summary
    { sThisWeek :: Map Day [WeeklyEpisode]
    , sAll :: [SeriesSummary]
    }
    deriving (Show)

data WeeklyEpisode = WeeklyEpisode
    { weSeriesName :: !Text
    , weAirDate :: !Day
    , weSeason :: !Integer
    , weEpisode :: !Integer
    }
    deriving (Show, Eq)

toEpNumber :: WeeklyEpisode -> Text
toEpNumber ep = (tshow . weSeason) ep <> "x" <> (tshow . weEpisode) ep

data SeriesSummary = SeriesSummary
    { ssName :: !Text
    , ssLastAir :: Maybe Day
    , ssNextAir :: Maybe Day
    }
    deriving (Show, Eq)

toWeeklyEpisode :: Series -> Episode -> WeeklyEpisode
toWeeklyEpisode s e =
    WeeklyEpisode
        { weSeriesName = sName s
        , weAirDate = eAirDate e
        , weSeason = eSeasonNumber e
        , weEpisode = eEpisodeNumber e
        }

toWeeklyEpisodes :: Series -> [WeeklyEpisode]
toWeeklyEpisodes s =
    catMaybes
        [ toWeeklyEpisode s <$> sLastEpisodeToAir s
        , toWeeklyEpisode s <$> sNextEpisodeToAir s
        ]

toThisWeek :: Day -> [WeeklyEpisode] -> Map Day [WeeklyEpisode]
toThisWeek day eps = fromListWith (++) eps'
  where
    (weekStart, weekEnd) = getWeekContaining day
    inWeek ep = weAirDate ep >= weekStart && weAirDate ep <= weekEnd
    eps' = fmap (\ep -> (weAirDate ep, [ep])) $ sortOn weAirDate $ filter inWeek eps

toSeriesSummary :: Series -> SeriesSummary
toSeriesSummary s =
    SeriesSummary
        { ssName = sName s
        , ssLastAir = eAirDate <$> sLastEpisodeToAir s
        , ssNextAir = eAirDate <$> sNextEpisodeToAir s
        }

toSummary :: Traversable f => Day -> f Series -> Summary
toSummary day sxs =
    Summary
        { sThisWeek = toThisWeek day $ concatMap toWeeklyEpisodes sxs
        , sAll = sortOn ssName $ toSeriesSummary <$> toList sxs
        }

getSummary :: (Traversable f) => Text -> f Text -> IO Summary
getSummary token sids = do
    sxs <- runReq defaultHttpConfig $ serieses token sids
    now <- today

    return $ toSummary now sxs

tshow :: (Show a) => a -> Text
tshow = pack . show

tshowOpt :: (Show a) => Maybe a -> Text
tshowOpt = maybe "?" tshow