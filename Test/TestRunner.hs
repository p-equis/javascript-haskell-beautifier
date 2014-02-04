module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified TokenizeTest

main :: IO ()
main = defaultMain tests

tests = TokenizeTest.tests
  