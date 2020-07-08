{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crossref
where

import           Control.Concurrent
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
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time
import           GHC.Generics
import qualified Text.Read                  as Read
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import           Network.URI

import           Crossref.Internal.API
import           Crossref.Internal.Env


crossRef :: Env -> T.Text -> IO (Either T.Text CrossrefSingleton)
crossRef Env {cache, rateLimitNextOkTime, accessLock, manager } doi = do
    putStrLn $ "Trying " ++ T.unpack doi
    fetchResult <- try $ cached cache doi hitAPI
    case fetchResult of
      Left (e :: Exception.SomeException) -> undefined
      Right a                             -> return (Right a)
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
                  Left e  -> undefined
                  Right a -> return a
