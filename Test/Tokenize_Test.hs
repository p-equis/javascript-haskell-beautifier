module Main (
    main
 ) where

import Tokenize
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain tests

shouldParseSimpleFunction :: Assertion
shouldParseSimpleFunction = assertEqual "should parse an empty function" 
	[Function, OpenParens, CloseParens, OpenBrace, CloseBrace]
	$ tokenize "function(){}"

shouldReturnNothingOnEmptyString :: Assertion
shouldReturnNothingOnEmptyString = assertEqual "should parse empty string" 
	[]
	$ tokenize ""

tests =
  [
    testCase "example test" shouldParseSimpleFunction,
    testCase "blah" shouldReturnNothingOnEmptyString
  ]