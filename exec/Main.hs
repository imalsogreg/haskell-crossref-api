{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crossref                   (DOI(..), ListQuery(..), lookupDOI, search, makeDefaultEnv)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Text                  as Text
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  env <- makeDefaultEnv
  let writeOutput x = case x of
        Right x' -> LazyByteString.putStrLn (Aeson.encode x')
        Left e   -> error (show e)
  case args of
    ["doi",  doiQ] -> lookupDOI env (DOI (Text.pack doiQ)) >>= writeOutput
    ["search", q]  -> search env (ListQuery { queryString = Text.pack q }) >>= writeOutput
    _     -> error "Usage: haskell-crossref-api doi"
