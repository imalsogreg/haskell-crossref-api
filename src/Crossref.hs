{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crossref (
    crossref
  , doi
  , search
  , makeDefaultEnv
 
  , DOI(..)
  , ListQuery(..)
  , CrossrefSingleton(..)
  , CrossrefList(..)
  , Error(..)

  , MessageList(..)
  , Message(..)
  , Contributor(..)
  , Affiliation(..)
  , Date(..)
  , PartialDate(..)
  , License(..)
  , Review(..)
  , ReviewStage(..)
  , reviewStages
  , ReviewRecommendation(..)
  , reviewRecommendations
  , ReviewType(..)
  , reviewTypes
  , DocumentType
  , documentTypes
  , documentTypeID

  ) where

import           Control.Concurrent
import qualified Data.Hashable as Hashable
import           Control.Concurrent.MVar
import           Control.Exception
import qualified Control.Exception          as Exception
import           Control.Monad
import qualified Data.Aeson                 as Aeson
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
import           Data.Time
import           GHC.Generics
import qualified Text.Read                  as Read
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import           Network.URI
import qualified System.LogLevel as LogLevel
import qualified Web.HttpApiData as HttpApiData

import           Crossref.Internal.API
import           Crossref.Internal.Env


class IsQuery q r | r -> q where
  cacheField         :: Env -> LruHandle q r
  toStructuredLog    :: q -> Maybe r -> Log
  queryToQueryString :: Proxy r -> q -> Text.Text

instance IsQuery DOI CrossrefSingleton where
  cacheField Env {cacheSingleton} = cacheSingleton
  toStructuredLog                 = undefined
  queryToQueryString _ doi        = "/works/" <> getDOI doi

instance IsQuery ListQuery CrossrefList where
  cacheField Env{ cacheList } = cacheList
  toStructuredLog             = undefined
  queryToQueryString _ q      = "/types/journal-article/works?rows=10&query=" <> HttpApiData.toQueryParam (queryString q)

doi :: Env -> DOI -> IO (Either Error CrossrefSingleton)
doi = crossref

search :: Env -> ListQuery -> IO (Either Error CrossrefList)
search = crossref

crossref :: forall q r.(IsQuery q r, Eq q, Ord q, Hashable.Hashable q, Aeson.FromJSON r) => Env -> q -> IO (Either Error r)
crossref
  env@Env
    { cacheSingleton
    , cacheList
    , rateLimitNextOkTime
    , accessLock
    , manager
    , logMessage
    }
  query =
  do
    -- logMessage LogLevel.Debug query Nothing ("Trying "  <> Text.pack (show getDOI))
    fetchResult <- try $ cached (cacheField env) query hitAPI
    case fetchResult of
      Left (e :: Error) -> do
        -- logMessage LogLevel.Error query (Just e) ""
        return $ Left e
      Right a -> return (Right a)
    where

      hitAPI :: IO r
      hitAPI = withMVar accessLock $ \() -> do
          now <- getCurrentTime
          tOk <- readIORef rateLimitNextOkTime
          threadDelay . floor $ 1000000 * realToFrac (min 1 $ max 0 $ diffUTCTime tOk now)
          let
            url = "https://api.crossref.org" <> queryToQueryString (Proxy :: Proxy r) query
              -- "https://api.crossref.org/works/" ++ Text.unpack getDOI
            request =
              Maybe.fromMaybe (throw (InvalidUrl url))
              $ HTTP.parseRequest (Text.unpack url)
          v <- Exception.try $ HTTP.httpLbs request manager
          case v of
              Left (e :: HTTP.HttpException) -> throw (HTTPError e)
              Right resp -> do
                let
                  limitHeader =
                    Map.lookup "X-Rate-Limit-Limit" (Map.fromList (HTTP.responseHeaders resp))
                case limitHeader >>= Read.readMaybe . BS.unpack of
                  Just rate -> do
                          writeIORef rateLimitNextOkTime
                                     (addUTCTime
                                      (realToFrac $ 1/ fromIntegral rate) tOk)
                          -- logMessage LogLevel.Info query Nothing $ Text.pack $ "Wrote rate " ++ show rate
                  _ -> return () -- logMessage LogLevel.Info query Nothing "No rate"
                print (HTTP.responseBody resp)
                case Aeson.eitherDecode (HTTP.responseBody resp) of
                  Left e  -> do
                    -- logMessage LogLevel.Error query (Just $ ParseError e) "Parse error"
                    throw (ParseError e)
                  Right a -> return a
