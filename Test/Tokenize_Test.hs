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

shouldOmitSpaces :: Assertion
shouldOmitSpaces = assertEqual "should omit spaces" 
	[OpenBrace, CloseBrace]
	$ tokenize "{ }"

tests =
  [
    testCase "example test" shouldParseSimpleFunction,
    testCase "blah" shouldReturnNothingOnEmptyString,
    testCase "should omit spaces" shouldOmitSpaces
  ]