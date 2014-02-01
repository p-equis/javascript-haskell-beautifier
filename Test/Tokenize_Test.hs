module Main (
    main
 ) where

import Tokenize( tokenize )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain tests

simpleTest :: Assertion
simpleTest = assertEqual "example assertion" 1 tokenize

tests =
  [
    testCase "example test" simpleTest
  ]