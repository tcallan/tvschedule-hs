module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Dhall (
    Decoder (expected),
    FromDhall,
    Generic,
    ToDhall,
    Vector,
    auto,
    defaultEvaluateSettings,
    inputFileWithSettings,
    substitutions,
 )
import Dhall.Map (insert)
import Lens.Micro.Platform (over)
import Summary (getSummary)
import Summary.Html (toHtml)
import Summary.Markdown (toMarkdown)

data OutputFormat
    = Html
    | Markdown
    deriving (Show, Generic, FromDhall, ToDhall)

data Config = Config {token :: Text, sIds :: Vector Text, format :: OutputFormat}
    deriving (Show, Generic, FromDhall, ToDhall)

main :: IO ()
main = do
    config <- inputFileWithSettings evaluateSettings auto "./config.dhall"
    summary <- getSummary (token config) (sIds config)

    T.putStrLn $ case format config of
        Html -> toHtml summary
        Markdown -> toMarkdown summary
  where
    evaluateSettings = over substitutions (insert "Format" formatType) defaultEvaluateSettings
    formatType = maximum $ expected (auto :: Decoder OutputFormat)