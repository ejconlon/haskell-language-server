{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Text (Text)
import Ide.Plugin.Simpletac (descriptor, Log)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Hls (goldenWithHaskellDoc, PluginTestDescriptor, mkPluginTestDescriptor, Session, TextDocumentIdentifier)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "simpletac"
  [ destructTest "single constructor" "t" 6 10 "DestructInt"
  ]

destructTest :: TestName -> Text -> Int -> Int -> FilePath -> TestTree
destructTest title _var _line _col input = goldenTest ("Destruct: " <> title) input $ \_tdi -> do
  pure () -- TODO apply tactic

goldenTest :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenTest title input = goldenWithHaskellDoc plugin title "test/golden" input "expected" "hs"

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "simpletac"
