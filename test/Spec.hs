{-# OPTIONS_GHC -Wno-all #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- Standard Library Imports
import Control.Monad (forM)
import Data.Function (on)
import Data.List (isSuffixOf, sort)
import Data.Tuple (swap)
import System.Console.ANSI (clearScreen)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath (takeExtension, dropExtension)

-- Third-Party Library Imports
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.Tasty.Muffled (muffledMain)
import Test.Tasty.QuickCheck ()
import Text.Megaparsec (parse, errorBundlePretty)

-- Project-Specific Imports
import Hurtle.Parser (parseHogo)
import Hurtle.Types ()

{-
    Tasty is the testing library that is used to specify tests.
    The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit 
    tests and property tests (respectively) are written.
-}

main :: IO ()
main = do

  passingFiles <- filter (\fp -> takeExtension fp == ".hogo") 
    <$> getDirectoryContents "examples/passing"
  passingCases <- forM (sort passingFiles) $ \fp -> do
    let fullPath = "examples/passing/" ++ fp
        expectedPath = dropExtension fullPath <> ".expected"
    hasExpected <- doesFileExist expectedPath
    let testName = if hasExpected then fp else fp ++ " (no expected output)"
    let test = testCase testName do
          input <- readFile fullPath
          if hasExpected then do
            shouldBe <- read <$> readFile expectedPath
            case parse parseHogo fp input of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right res -> res @?= shouldBe
          else 
            case parse parseHogo fp input of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right _ -> pure () -- Assume all good

    return test
  failingFiles <- filter (".hogo" `isSuffixOf`) 
    <$> getDirectoryContents "examples/failing"
  failingCases <- forM (sort failingFiles) $ \fp -> do
    let fullPath = "examples/failing/" ++ fp
    let test = testCase fp do
          input <- readFile fullPath
          case parse parseHogo fp input of
            Left _ -> return ()
            Right res -> assertFailure 
              $ "Expected parse error, but parsing was successful.\n"
                <> "Result was the following program:\n" 
                <> show res
    return test

  clearScreen
  muffledMain $
    testGroup
      "Examples"
      [ testGroup "Passing cases (should all parse correctly)" passingCases
      , testGroup "Failing cases (should all fail to parse)" failingCases
      ]
