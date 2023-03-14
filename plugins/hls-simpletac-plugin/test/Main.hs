{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Text (Text)
import Ide.Plugin.Simpletac (Simpletac (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "simpletac"
  [ destructTest "t" 6 10 "DestructInt"
  ]

destructTest :: Text -> Int -> Int -> FilePath -> TestTree
destructTest var line col = goldenTest (SimpletacDestruct var line col)

goldenTest :: Simpletac -> FilePath -> TestTree
goldenTest _tac input = testCase (input <> " (golden)") $ do
  -- doc <- openDoc (input <.> "hs") "haskell"
  -- traverse_ (invokeTactic doc) invocations
  -- edited <- documentContents doc
  -- let expected_name = input <.> "expected" <.> "hs"
  -- -- Write golden tests if they don't already exist
  -- liftIO $ (doesFileExist expected_name >>=) $ flip unless $ do
  --     T.writeFile expected_name edited
  -- expected <- liftIO $ T.readFile expected_name
  -- liftIO $ edited `eq` expected
  pure () -- TODO
