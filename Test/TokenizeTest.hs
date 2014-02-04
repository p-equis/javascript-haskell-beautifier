module TokenizeTest (
    tests
 ) where

import Tokenize
import Token
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

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

shouldOmitTabs :: Assertion
shouldOmitTabs = assertEqual "should omit tabs"
	[OpenBrace, CloseBrace]
	$ tokenize "{\t}"

shouldParseReturnStatement :: Assertion
shouldParseReturnStatement = assertEqual "should parse return statement" 
	[OpenBrace, Return, CloseBrace]
	$ tokenize "{ return }"

tests =
  [
    testCase "example test" shouldParseSimpleFunction,
    testCase "blah" shouldReturnNothingOnEmptyString,
    testCase "should omit spaces" shouldOmitSpaces,
    testCase "should omit tabs" shouldOmitTabs,
    testCase "should parse return statement" shouldParseReturnStatement
  ]