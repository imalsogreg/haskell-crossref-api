{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crossref.Internal.Env where

import           Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.Aeson              as Aeson
import qualified Data.IORef              as IORef
import           Data.LruCache
import           Data.LruCache.IO
import           Data.Text               (Text)
import           Data.Time               (UTCTime, getCurrentTime)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           System.IO               (hPutStrLn, stderr)
import qualified System.LogLevel         as LogLevel

import           Crossref.Internal.API   (CrossrefSingleton, DOI, Error(..), ListQuery(..), CrossrefList(..), Log(..))




data Env = Env
  { manager             :: Manager
  , cacheSingleton      :: LruHandle DOI   CrossrefSingleton
  , cacheList           :: LruHandle ListQuery CrossrefList
  , rateLimitNextOkTime :: IORef.IORef UTCTime
  , accessLock          :: MVar ()
  , logMessage          :: Log -> IO ()
  }

instance Show Env where
  show _ = "\"<crossref env>\""

makeDefaultEnv :: IO Env
makeDefaultEnv = do
    rateLimitNextOkTime  <- IORef.newIORef =<< getCurrentTime
    accessLock           <- newMVar ()
    manager              <- newTlsManager
    cacheSingleton       <- newLruHandle 500
    cacheList            <- newLruHandle 500
    let
      logMessage Log{ level, query, error_ } =
        hPutStrLn stderr $ show (level, query, error_)
    return $ Env { manager, cacheSingleton, cacheList, rateLimitNextOkTime, accessLock, logMessage }
