{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crossref
where

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
import qualified Data.Text                  as T
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

-- import           Crossref.Internal
import           Crossref.Internal.API
import           Crossref.Internal.Env

-- type CrossrefAPI = "works"
--     :> Capture "doi" T.Text
--     :> Get '[JSON] (Headers '[Header "X-Rate-Limit-Limit" Int] CrossrefSingleton)

-- api :: Proxy CrossrefAPI
-- api = Proxy

crossRef :: Env -> T.Text -> IO (Either T.Text CrossrefSingleton)
crossRef Env {cache, rateLimitNextOkTime, accessLock, manager } doi = do
    putStrLn $ "Trying " ++ T.unpack doi
    fetchResult <- try $ cached cache doi hitAPI
    case fetchResult of
      Left (e :: Exception.SomeException) -> undefined
      Right a -> return (Right a)
    -- fmap (fmapL (\(e :: SomeException) -> T.pack $ show e)) $
    --               try (cached (cache cr) doi hitAPI)
    where
      hitAPI :: IO CrossrefSingleton
      hitAPI = withMVar accessLock $ \() -> do
          now <- getCurrentTime
          tOk <- readIORef rateLimitNextOkTime
          threadDelay . floor $ 1000000 * realToFrac (min 1 $ max 0 $ diffUTCTime tOk now)
          let request = Maybe.fromMaybe ("request parse failure")
                $ HTTP.parseRequest
                $ "https://api.crossref.org/works/" ++ T.unpack doi
          v <- Exception.try $ HTTP.httpLbs request manager
          -- v <- runClientM (client api doi) (ClientEnv (crManager cr) apiBase Nothing)
          case v of
              Left (e :: Exception.SomeException)    -> error (show e)
              Right resp -> do
                let
                  limitHeader =
                    Map.lookup "X-Rate-Limit-Limit" (Map.fromList (HTTP.responseHeaders resp))
                case limitHeader >>= Read.readMaybe . BS.unpack of
                  Just rate -> do
                          writeIORef rateLimitNextOkTime
                                     (addUTCTime
                                      (realToFrac $ 1/ fromIntegral rate) tOk)
                          putStrLn $ "Wrote rate " ++ show rate
                  _ -> putStrLn "No rate"
                case Aeson.eitherDecode (HTTP.responseBody resp) of
                  Left e -> undefined
                  Right a -> return a





-- parseDocumentHints :: CrossrefSingleton -> DocumentHints
-- parseDocumentHints (CrossrefSingleton msg _ _) = DocumentHints
--   { titleHint   = Maybe.fromMaybe (error "No Crossref title") . listToMaybe $ m_title msg
--   , authorsHint = (\(Author giv fam) -> T.unwords [giv, fam]) <$> m_authors msg
--   , yearHint    = (\(y,_,_) -> Just (fromIntegral y)) . toGregorian . unCrossrefDate $ m_created msg
--   , linkHint    = m_URL msg
--   }

-- data CrossrefSingleton = CrossrefSingleton
--   { message        :: CrossrefMessage
--   , status         :: T.Text
--   , messageVersion :: T.Text
--   } deriving (Eq, Ord, Show, Generic)


-- instance Aeson.FromJSON CrossrefSingleton where
--   parseJSON = Aeson.withObject "CrossrefSingleton" $ \o -> CrossrefSingleton
--     <$> o Aeson..: "message"
--     <*> o Aeson..: "status"
--     <*> o Aeson..: "message-version"


-- data CrossrefMessage = CrossrefMessage
--   { m_authors :: [Author]
--   , m_title   :: [T.Text]
--   , m_URL     :: T.Text
--   , m_subejct :: [T.Text]
--   , m_created :: CrossrefDate
--   } deriving (Eq, Ord, Show, Generic)

-- instance Aeson.FromJSON CrossrefMessage where
--     parseJSON = Aeson.withObject "CrossrefMessage" $ \o -> CrossrefMessage
--       <$> o Aeson..: "author"
--       <*> o Aeson..: "title"
--       <*> o Aeson..: "URL"
--       <*> o Aeson..: "subject"
--       <*> o Aeson..: "created"

-- data Author = Author
--   { given  :: T.Text
--   , family :: T.Text
--   } deriving (Eq, Ord, Show)

-- instance Aeson.FromJSON Author where
--     parseJSON = Aeson.withObject "CrossrefAuthor" $ \o -> Author
--         <$> o Aeson..: "given"
--         <*> o Aeson..: "family"

-- data CrossrefDate = CrossrefDate { unCrossrefDate :: Day }
--     deriving (Eq, Ord, Show)

-- instance Aeson.FromJSON CrossrefDate where
--     parseJSON = Aeson.withObject "CrossrefDate" $ \o -> CrossrefDate <$> do
--         parts <- o Aeson..: "date-parts"
--         case parts of
--             ([y,m,d]:_) -> return $ fromGregorian (fromIntegral y) m d
--             _           -> mzero
