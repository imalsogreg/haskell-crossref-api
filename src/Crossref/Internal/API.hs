{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crossref.Internal.API where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import           Control.Concurrent
import qualified Text.Read as Read
import           Control.Concurrent.MVar
import qualified Data.Maybe as Maybe
import           Control.Exception
import qualified Data.Map.Strict as Map
import           Control.Monad
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as C
import           Data.IORef
import qualified Data.List                  as L
import           Data.LruCache
import           Data.LruCache.IO
import           Data.Maybe                 (catMaybes, isJust, listToMaybe)
import           Data.Proxy
import qualified Data.Text                  as Text
import           Data.Text.Encoding
import qualified Control.Exception as Exception
import           Data.Time
import           GHC.Generics
-- import           Network.Browser            hiding (Proxy (..))
-- import qualified Network.HTTP               as H
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import           Network.URI
-- import           Safe



-- type CrossrefAPI = "works"
--     :> Capture "doi" Text.Text
--     :> Get '[JSON] (Headers '[Header "X-Rate-Limit-Limit" Int] CrossrefSingleton)

-- api :: Proxy CrossrefAPI
-- api = Proxy




-- data DocumentHints = DocumentHints
--  { titleHint   :: Text.Text
--  , authorsHint :: [Text.Text]
--  , linkHint    :: Text.Text
--  , license     :: License
--  , yearHint    :: Maybe Int
--  } deriving (Show, Generic)

-- instance Aeson.ToJSON DocumentHints



-- parseDocumentHints :: CrossrefSingleton -> DocumentHints
-- parseDocumentHints (CrossrefSingleton msg _ _) = DocumentHints
--   { titleHint   = Maybe.fromMaybe (error "No Crossref title") . listToMaybe $ m_title msg
--   , authorsHint = (\(Author giv fam) -> Text.unwords [giv, fam]) <$> m_authors msg
--   , yearHint    = (\(y,_,_) -> Just (fromIntegral y)) . toGregorian . unCrossrefDate $ m_created msg
--   , linkHint    = m_URL msg
--   , license
--   }

data CrossrefSingleton = CrossrefSingleton
  { message        :: CrossrefMessage
  , status         :: Text.Text
  , messageVersion :: Text.Text
  } deriving (Show, Generic)


instance Aeson.FromJSON CrossrefSingleton where
  parseJSON (Aeson.Object o) = CrossrefSingleton
    <$> o Aeson..: "message"
    <*> o Aeson..: "status"
    <*> o Aeson..: "message-version"


data CrossrefMessage = CrossrefMessage
  { m_authors  :: [Author]
  , m_title    :: [Text.Text]
  , m_URL      :: Text.Text
  , m_subejct  :: [Text.Text]
  , m_created  :: CrossrefDate
  , m_licenses :: [License]
  } deriving (Show, Generic)

instance Aeson.FromJSON CrossrefMessage where
    parseJSON (Aeson.Object o) = CrossrefMessage
      <$> o Aeson..: "author"
      <*> o Aeson..: "title"
      <*> o Aeson..: "URL"
      <*> o Aeson..: "subject"
      <*> o Aeson..: "created"
      <*> (o Aeson..: "license" <|> pure [])


data Author = Author
  { given  :: Text.Text
  , family :: Text.Text
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Author where
    parseJSON = Aeson.withObject "CrossrefAuthor" $ \o -> Author
        <$> o Aeson..: "given"
        <*> o Aeson..: "family"

data CrossrefDate = CrossrefDate { unCrossrefDate :: Day }
    deriving (Eq, Ord, Show)

instance Aeson.FromJSON CrossrefDate where
    parseJSON (Aeson.Object o) = CrossrefDate <$> do
        parts <- o Aeson..: "date-parts"
        case parts of
            ([y,m,d]:_) -> return $ fromGregorian (fromIntegral y) m d
            _           -> mzero
    parseJSON _ = mzero

data License = License
  { url :: Text.Text
  , delayInDays :: Int
  , contentVersion :: Text.Text
  } deriving (Show, Generic)

instance Aeson.FromJSON License where
  parseJSON = Aeson.withObject "License" $ \o -> License
    <$> o Aeson..: "URL"
    <*> o Aeson..: "delay-in-days"
    <*> o Aeson..: "content-version"

instance Aeson.ToJSON License where
  toJSON License { url, delayInDays, contentVersion } = Aeson.object
    ["URL"              Aeson..= url
    ,"delay-in-days"    Aeson..= delayInDays
    , "content-version" Aeson..= contentVersion
    ]

-- "license": [
--       {
--         "URL": "http://creativecommons.org/licenses/by/4.0/",
--         "start": {
--           "date-parts": [
--             [
--               2020,
--               1,
--               21
--             ]
--           ],
--           "date-time": "2020-01-21T00:00:00Z",
--           "timestamp": 1579564800000
--         },
--         "delay-in-days": 0,
--         "content-version": "vor"
--       }
--     ],
