module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Dhall (FromDhall, Generic, Vector, auto, input)
import Summary (getSummary)
import Summary.Markdown (toMarkdown)

data Config = Confg {token :: Text, sIds :: Vector Text} deriving (Show, Generic)

instance FromDhall Config

main :: IO ()
main = do
  config <- input auto "./config.dhall"
  summary <- getSummary (token config) (sIds config)

  T.putStrLn $ toMarkdown summary