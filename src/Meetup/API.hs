{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Meetup.API where

import           Data.Text      (Text)

import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Meetup.Data

type RateLimits a = Headers '[Header "X-RateLimit-Limit" Int
                             ,Header "X-RateLimit-Remaining" Int
                             ,Header "X-RateLimit-Reset" Int
                             ] a

type API = Capture "urlname" Text :>
                   "events" :>
                   QueryParam "desc" Bool :>
                   QueryParams "fields" Text :>
                   QueryParam "page" Int :>
                   QueryParam "scroll" Scroll :>
                   QueryParams "status" Status :>
                   QueryParam "key" Text :>
                   Get '[JSON] (RateLimits [Event])

api :: Proxy API
api = Proxy

groupEvents :: Text
            -> Maybe Bool
            -> [Text]
            -> Maybe Int
            -> Maybe Scroll
            -> [Status]
            -> Maybe Text
            -> ClientM
                 (Headers
                    '[Header "X-RateLimit-Limit" Int,
                      Header "X-RateLimit-Remaining" Int, Header "X-RateLimit-Reset" Int]
                    [Event])
groupEvents = client api

minimalGroupEvents :: Text -> Maybe Text -> ClientM (RateLimits [Event])
minimalGroupEvents group key = groupEvents group Nothing [] Nothing Nothing [] key
