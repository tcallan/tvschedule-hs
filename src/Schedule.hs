module Schedule (
    today,
    isInWeekContaining,
    getWeekContaining,
) where

import Data.Functor ((<&>))
import Data.Time (Day, UTCTime (utctDay), addDays, getCurrentTime)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)

today :: IO Day
today = getCurrentTime <&> utctDay

isInWeekContaining :: Day -> Day -> Bool
isInWeekContaining date check = check >= weekStart && check <= weekEnd
  where
    (weekStart, weekEnd) = getWeekContaining date

-- | Get a tuple of days representing the start and end of the week containing the provided day
getWeekContaining :: Day -> (Day, Day)
getWeekContaining date = (weekStart, weekEnd)
  where
    (year, week, day) = toWeekDate date
    -- time's weeks run Monday to Sunday, but we want Sunday to Saturday so need to consider
    -- Sunday to be part of the next week
    properWeek = if day == 7 then week + 1 else week
    weekStart = addDays (-1) $ fromWeekDate year properWeek 1
    weekEnd = addDays (-1) $ fromWeekDate year properWeek 7