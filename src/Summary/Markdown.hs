module Summary.Markdown (toMarkdown) where

import Data.Map.Strict (Map, foldlWithKey)
import Data.Text (Text)
import Data.Time (Day, dayOfWeek)
import Summary (SeriesSummary (..), Summary (..), WeeklyEpisode (..), toEpNumber, tshow, tshowOpt)

toMarkdown :: Summary -> Text
toMarkdown s = toMarkdownThisWeek (sThisWeek s) <> toMarkdownAll (sAll s)

toMarkdownThisWeek :: Map Day [WeeklyEpisode] -> Text
toMarkdownThisWeek = foldlWithKey folder "# This Week\n\n"
  where
    folder current day eps = current <> toMarkdownDay day eps

toMarkdownDay :: (Foldable t) => Day -> t WeeklyEpisode -> Text
toMarkdownDay day eps = foldl f header eps <> "\n"
  where
    header = "## " <> (tshow . dayOfWeek) day <> " (" <> tshow day <> ")\n"
    f current ep = current <> "- " <> weSeriesName ep <> " " <> toEpNumber ep <> "\n"

toMarkdownAll :: [SeriesSummary] -> Text
toMarkdownAll = foldl f header
  where
    header = "# Currently Watching\n\n"
    f current s =
        current <> "## " <> ssName s <> "\n"
            <> ("- Last: " <> (tshowOpt . ssLastAir) s <> "\n")
            <> ("- Next: " <> (tshowOpt . ssNextAir) s <> "\n\n")