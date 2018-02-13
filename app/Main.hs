{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Semigroup          ((<>))
import           Data.Text               hiding (group)
import           Meetup.API
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           Servant.API
import           Servant.Client

data CommandLineOptions = CommandLineOptions { clo_apiKey :: Maybe Text
                                             , clo_group  :: Text } deriving (Eq, Show)

apiKey :: Parser Text
apiKey = strOption
            (long "apiKey" <>
             short 'k' <>
             help "Your API key to use for requests to the meetup api. You can get it here: https://secure.meetup.com/meetup_api/key")

group :: Parser Text
group = strOption
           (long "group" <>
            short 'g' <>
            help "The meetup group you want to receive the events for")

commandLineOptions :: Parser CommandLineOptions
commandLineOptions = CommandLineOptions <$> optional apiKey <*> group

opts :: ParserInfo CommandLineOptions
opts = info (commandLineOptions <**> helper)
  (fullDesc <>
   progDesc "Manage your Meetup Events through the command line!" <>
   header "meetup-fst - a haskell Meetup Events manipulator on the command line")

main :: IO ()
main = do
  clo <- execParser opts
  m <- newManager tlsManagerSettings
  let clientEnv = ClientEnv m (BaseUrl Https "api.meetup.com" 443 "")
  res <- runClientM (minimalGroupEvents (clo_group clo) (clo_apiKey clo)) clientEnv
  case res of
    (Left err) -> do
      putStrLn "Some error occurred:"
      print err
    (Right result) -> do
      putStrLn "Result:"
      print $ getResponse result
      putStrLn "Rate limiting info:"
      print $ getHeaders result
