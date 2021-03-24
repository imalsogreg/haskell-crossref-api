{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.FilePath ((</>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Test.Tasty
import Test.Tasty.HUnit

import Crossref
import qualified Paths_crossref as Paths

main :: IO ()
main = defaultMain $ testGroup "encoding tests"
  [ testCase "parse single work" $ do
      testFile <- Paths.getDataFileName $ "testdata" </> "works_10.1101_2020.08.17.253575.json"
      Just (CrossrefSingleton { messageVersion, message = Work { title } }) <- Aeson.decode <$> LazyByteString.readFile testFile
      messageVersion @?= "1.0.0"
      title @?= ["Role of neutrophil extracellular traps in regulation of lung cancer invasion and metastasis: Structural Insights from a Computational Model"]

  , testCase "parse search result" $ do
      testFile <- Paths.getDataFileName $ "testdata" </> "query_hippocampal_place_cells.json"
      Just (CrossrefList { messageVersion, message = WorkList { totalResults, items }}) <- Aeson.decode <$> LazyByteString.readFile testFile
      title (items !! 2) @?= ["Place Cells and Place Recognition Maintained by Direct Entorhinal-Hippocampal Circuitry"]
      let Link { url } = ((!! 1) . link . (!!5) $ items )
      url @?= "https://api.elsevier.com/content/article/PII:S0896627304002193?httpAccept=text/plain"
  ]
