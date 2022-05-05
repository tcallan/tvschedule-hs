module Summary.Html (toHtml) where

import Control.Monad (forM_)
import Data.Map.Strict (Map, toList)
import Data.Text (Text, pack)
import Data.Time (Day, dayOfWeek)
import Summary (SeriesSummary (..), Summary (..), WeeklyEpisode (..))
import qualified Text.Blaze.Html.Renderer.Pretty as H
import qualified Text.Blaze.Html5 as H

toHtml :: Summary -> Text
toHtml s = pack $
    H.renderHtml $
        H.html $ do
            H.head $ do
                H.title "TV Schedule"
            H.body $ do
                toHtmlThisWeek (sThisWeek s)
                toHtmlAll (sAll s)

toHtmlThisWeek :: Map Day [WeeklyEpisode] -> H.Html
toHtmlThisWeek days = do
    H.h1 "This Week"
    forM_ (toList days) (uncurry toHtmlThisWeekDay)

toHtmlThisWeekDay :: Day -> [WeeklyEpisode] -> H.Html
toHtmlThisWeekDay day eps = do
    H.h2 $ H.toHtml $ tshow (dayOfWeek day) <> " (" <> tshow day <> ")"
    H.ul $ forM_ eps toHtmlWeeklyEpisode

toHtmlWeeklyEpisode :: WeeklyEpisode -> H.Html
toHtmlWeeklyEpisode ep =
    H.li $
        H.toHtml $
            weSeriesName ep <> " " <> (tshow . weSeason) ep <> "x" <> (tshow . weEpisode) ep

toHtmlAll :: [SeriesSummary] -> H.Html
toHtmlAll sxs = do
    H.h1 "Currently Watching"
    forM_ sxs toHtmlSeries

toHtmlSeries :: SeriesSummary -> H.Html
toHtmlSeries s = do
    H.h1 $ H.toHtml $ ssName s
    H.ul $ do
        H.li $ H.toHtml $ "Last: " <> toHtmlOpt (ssLastAir s)
        H.li $ H.toHtml $ "Next: " <> toHtmlOpt (ssNextAir s)

tshow :: (Show a) => a -> Text
tshow = pack . show

toHtmlOpt :: (Show a) => Maybe a -> Text
toHtmlOpt = maybe "?" tshow