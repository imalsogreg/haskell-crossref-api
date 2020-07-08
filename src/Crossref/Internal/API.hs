-- | Serialization for the types defined in
--   https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#relation

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Crossref.Internal.API where

import           Control.Applicative        ((<|>))
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import qualified Control.Exception          as Exception
import           Control.Monad
import qualified Control.Monad.Fail         as Fail
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Char                  as C
import           Data.IORef
import qualified Data.List                  as L
import           Data.LruCache
import           Data.LruCache.IO
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, isJust, listToMaybe)
import qualified Data.Maybe                 as Maybe
import           Data.Proxy
import qualified Data.Text                  as Text
import           Data.Text.Encoding
import qualified Data.Time                  as Time
import qualified Data.Vector                as Vector
import           GHC.Generics
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import           Network.URI
import qualified Text.Read                  as Read




data CrossrefSingleton = CrossrefSingleton
  { message        :: Message
  , status         :: Text.Text
  , messageVersion :: Text.Text
  } deriving (Show, Generic)


instance Aeson.FromJSON CrossrefSingleton where
  parseJSON (Aeson.Object o) = CrossrefSingleton
    <$> o Aeson..: "message"
    <*> o Aeson..: "status"
    <*> o Aeson..: "message-version"


data Message = Message
  { authors  :: [Contributor]
  , title    :: [Text.Text]
  , url      :: Text.Text
  , subejct  :: [Text.Text]
  , created  :: Date
  , licenses :: [License]
  } deriving (Show, Generic)

instance Aeson.FromJSON Message where
    parseJSON (Aeson.Object o) = Message
      <$> o Aeson..: "author"
      <*> o Aeson..: "title"
      <*> o Aeson..: "URL"
      <*> o Aeson..: "subject"
      <*> o Aeson..: "created"
      <*> (o Aeson..: "license" <|> pure [])


data Contributor = Contributor
  { given       :: Maybe Text.Text
  , family      :: Text.Text
  , orcid       :: Maybe Text.Text
  , affiliation :: [Affiliation]
  } deriving (Eq, Ord, Show)


instance Aeson.FromJSON Contributor where
    parseJSON = Aeson.withObject "CrossrefAuthor" $ \o -> Contributor
        <$>  o Aeson..:  "given"
        <*>  o Aeson..:  "family"
        <*>  o Aeson..:? "orcid"
        <*> (o Aeson..:  "affiliation" <|> pure [])

data Affiliation = Affiliation { name :: Text.Text }
  deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Affiliation where
  parseJSON = Aeson.withObject "Affiliation" $ \o ->
    Affiliation <$> o Aeson..: "name"

instance Aeson.ToJSON Affiliation where
  toJSON Affiliation{ name } = Aeson.object ["name" Aeson..= name ]

data Date = Date { unDate :: Time.Day }
    deriving (Eq, Ord, Show)

instance Aeson.FromJSON Date where
    parseJSON = Aeson.withObject "Date" $ \o -> do
        parts <- o Aeson..: "date-parts"
        case parts of
            ([y,m,d]:_) -> return $ Date $ Time.fromGregorian (fromIntegral y) m d
            _           -> mzero

instance Aeson.ToJSON Date where
  toJSON (Time.toGregorian . unDate -> (y,m,d)) = Aeson.object
    ["date-parts" Aeson..= [fromIntegral y,m,d]]

data PartialDate = PartialDate
  { year  :: Int
  , month :: Maybe Int
  , day   :: Maybe Int
  } deriving (Eq, Ord, Show, Generic)

instance Aeson.FromJSON PartialDate where
  parseJSON = Aeson.withObject "PartialDate" $ \o -> do
    parts :: [Int] <- o Aeson..: "date-parts"
    case parts of
          [y, m, d] -> pure $ PartialDate { year = y, month = Just m,  day = Just d  }
          [y, m]    -> pure $ PartialDate { year = y, month = Just m,  day = Nothing }
          [y]       -> pure $ PartialDate { year = y, month = Nothing, day = Nothing }
          _         -> Fail.fail $ "malformatted date-parts: " ++ show parts


instance Aeson.ToJSON PartialDate where
  toJSON PartialDate { year, month, day } = Aeson.object
    ["date-parts" Aeson..= Maybe.catMaybes [Just year, month, day]]


data License = License
  { url            :: Text.Text
  , delayInDays    :: Int
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


-- * Review

data Review = Review
  { runningNumber              :: Maybe Text.Text
  , revisionRound              :: Maybe Text.Text
  , stage                      :: Maybe ReviewStage
  , recommendation             :: Maybe ReviewRecommendation
  , type_                      :: Maybe ReviewType
  , competingInterestStatement :: Maybe Text.Text
  , language                   :: Maybe Text.Text
  } deriving (Show, Generic)


instance Aeson.FromJSON Review where
  parseJSON = Aeson.withObject "Review" $ \o -> Review
    <$> o Aeson..:? "running-number"
    <*> o Aeson..:? "revision-round"
    <*> o Aeson..:? "stage"
    <*> o Aeson..:? "recommendation"
    <*> o Aeson..:? "type"
    <*> o Aeson..:? "competing-interest-statement"
    <*> o Aeson..:? "language"

instance Aeson.ToJSON Review where
  toJSON Review { runningNumber
                , revisionRound
                , stage
                , recommendation
                , type_
                , competingInterestStatement
                , language
                } = Aeson.object
    [ "running-number"               Aeson..= runningNumber
    , "revision-round"               Aeson..= revisionRound
    , "stage"                        Aeson..= stage
    , "recommendation"               Aeson..= recommendation
    , "type"                         Aeson..= type_
    , "competing-interest-statement" Aeson..= competingInterestStatement
    , "language"                     Aeson..= language]


data ReviewStage = PrePublication | PostPublication
  deriving (Show, Generic)


parseEnum :: Text.Text -> [(Text.Text, a)] -> Aeson.Value -> Aeson.Parser a
parseEnum enumName levels = Aeson.withText (Text.unpack enumName) $ \t ->
  case L.lookup t levels of
    Just a  -> pure a
    Nothing -> Fail.fail $
      Text.unpack $ "must be one of: " <> Text.intercalate ", " (fst <$> levels)

reviewStages :: [(Text.Text, ReviewStage)]
reviewStages = [ ("pre-publication",  PrePublication )
              , ("post-publication", PostPublication)
              ]

instance Aeson.FromJSON ReviewStage where
  parseJSON = parseEnum "ReviewStage" reviewStages

instance Aeson.ToJSON ReviewStage where
  toJSON PrePublication  = "pre-publication"
  toJSON PostPublication = "post-publication"

data ReviewRecommendation
  = MajorRevision
  | MinorRevision
  | Reject
  | RejectWithResubmit
  | Accept
  deriving (Eq, Enum, Show, Generic)

reviewRecommendations :: [(Text.Text, ReviewRecommendation)]
reviewRecommendations =
  [("major-revision", MajorRevision)
  ,("minor-revision", MinorRevision)
  ,("reject", Reject)
  ,("reject-with-resubmit", RejectWithResubmit)
  ,("accept", Accept)
  ]

instance Aeson.FromJSON ReviewRecommendation where
  parseJSON = parseEnum "ReviewRecommendation" reviewRecommendations

instance Aeson.ToJSON ReviewRecommendation where
  toJSON MajorRevision      = "major-revision"
  toJSON MinorRevision      = "minor-revision"
  toJSON Reject             = "reject"
  toJSON RejectWithResubmit = "reject-with-resubmit"
  toJSON Accept             = "accept"


data ReviewType
  = RefereeReport
  | EditorReport
  | AuthorComment
  | CommunityComment
  | Aggregate
  deriving (Eq, Enum, Show, Generic)

reviewTypes :: [(Text.Text, ReviewType)]
reviewTypes =
  [("referee-report",    RefereeReport   )
  ,("editor-report",     EditorReport    )
  ,("author-comment",    AuthorComment   )
  ,("community-comment", CommunityComment)
  ,("aggregate",         Aggregate       )
  ]

instance Aeson.FromJSON ReviewType where
  parseJSON = parseEnum "ReviewType" reviewTypes

instance Aeson.ToJSON ReviewType where
  toJSON RefereeReport    = "referee-report"
  toJSON EditorReport     = "editor-report"
  toJSON AuthorComment    = "author-comment"
  toJSON CommunityComment = "community-comment"
  toJSON Aggregate        = "aggregate"
