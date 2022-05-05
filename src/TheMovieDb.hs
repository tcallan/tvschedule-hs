module TheMovieDb (series, serieses, Series (..), Episode (..)) where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day)
import GHC.Generics (Generic)
import Network.HTTP.Req (
    GET (GET),
    MonadHttp,
    NoReqBody (NoReqBody),
    https,
    jsonResponse,
    oAuth2Bearer,
    req,
    responseBody,
    (/:),
 )

serieses :: (MonadHttp m, Traversable f) => Text -> f Text -> m (f Series)
serieses token = mapM (series token)

series :: (MonadHttp m) => Text -> Text -> m Series
series token sid = do
    resp <- req GET uri body jsonResponse auth
    return $ responseBody resp
  where
    body = NoReqBody
    uri = https "api.themoviedb.org" /: "3" /: "tv" /: sid
    auth = oAuth2Bearer . encodeUtf8 $ token

data Series = Series
    { sId :: !Integer
    , sName :: !Text
    , sLastEpisodeToAir :: Maybe Episode
    , sNextEpisodeToAir :: Maybe Episode
    }
    deriving (Show, Generic)

instance FromJSON Series where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Episode = Episode
    { eAirDate :: !Day
    , eEpisodeNumber :: !Integer
    , eId :: !Integer
    , eName :: !Text
    , eOverview :: !Text
    , eSeasonNumber :: !Integer
    , eRuntime :: Maybe Integer
    }
    deriving (Show, Generic)

instance FromJSON Episode where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase