module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain tests

simpleTest :: Assertion
simpleTest = assertEqual "preface" 1 1

tests =
  [
    testCase "example" simpleTest
  ]