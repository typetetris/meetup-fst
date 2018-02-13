{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Meetup.API.Internal

( genericToJSONString
, genericToEncodingString
, genericParseJSONString
)

where

import           Data.Aeson
import           Data.Aeson.Encoding (text)
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M (fromList, lookup)
import           Data.List           (intersperse)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import           Data.Text           as T (Text, pack, unpack, unwords)
import           GHC.Generics

class GAllValues f where
  allValues' :: [ f a ]

instance GAllValues U1 where
  allValues' = [ U1 ]

instance (GAllValues a, GAllValues b) => GAllValues (a :+: b) where
  allValues' = (L1 <$> allValues') ++ (R1 <$> allValues')

instance (GAllValues a, GAllValues b) => GAllValues (a :*: b) where
  allValues' = (:*:) <$> allValues' <*> allValues'

instance GAllValues a => GAllValues (M1 i c a) where
  allValues' = M1 <$> allValues'

instance (Generic a, GAllValues (Rep a)) => GAllValues (K1 i a) where
  allValues' = K1 <$> allValues

allValues :: (Generic a, GAllValues (Rep a)) => [ a ]
allValues = to <$> allValues'

genericToJSONString :: (a -> Text) -> a -> Value
genericToJSONString f val = String $ f val
genericToEncodingString :: (a -> Text) -> a -> Encoding
genericToEncodingString f value = text (f value)

ellipsedAlternative :: Int -> [Text] -> Text
ellipsedAlternative n values = let ellipse = if length (take (n+1) values) == n + 1 then [ "…" ] else []
                               in T.unwords . intersperse "|" $ (take n values) ++ ellipse

genericParseJSONString :: (Rep a ~ t (d :: k) (f :: * -> *), Generic a, Datatype d, GAllValues (Rep a)) => (a -> Text) -> Value -> Parser a
genericParseJSONString f = let values = [(f x, x) | x <- allValues]
                               revMap = M.fromList values
                               name = pack . datatypeName . from . snd . head $ values
                               errMsgValues = [ "\"" <> t <> "\"" | (t, _) <- values ]
                               errMsg = unpack $ "expected for " <> name <> " one of the following JSON values " <> ellipsedAlternative 3 errMsgValues
                           in withText errMsg $ \t -> fromMaybe (fail errMsg) (return <$> (M.lookup t revMap))
