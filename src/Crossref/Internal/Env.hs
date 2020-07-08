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

import Crossref.Internal.API (CrossrefSingleton)


makeDefaultEnv :: IO Env
makeDefaultEnv = do
    rateLimitNextOkTime  <- IORef.newIORef =<< getCurrentTime
    accessLock <- newMVar ()
    manager   <- newTlsManager
    cache <- newLruHandle 500
    return $ Env { manager, cache, rateLimitNextOkTime, accessLock }


data Env = Env
  { manager             :: Manager
  , cache               :: LruHandle Text CrossrefSingleton
  , rateLimitNextOkTime :: IORef.IORef UTCTime
  , accessLock          :: MVar ()
  }

instance Show Env where
  show _ = "\"<crossref env>\""
