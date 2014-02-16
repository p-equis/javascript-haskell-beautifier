module TokenizeSpec (spec) where

import Test.Hspec
import Tokenize
import Token

spec :: Spec
spec = do
	describe "tokenizing" $ do
		it "should parse an empty function" $ do
			tokenize "function(){}" `shouldBe` [Function, OpenParens, CloseParens, OpenBrace, CloseBrace]

		it "should parse empty string" $ do
			tokenize "" `shouldBe` []

		it "should omit spaces" $ do
			tokenize "{ }" `shouldBe` [OpenBrace, CloseBrace]
	
		it "should omit tabs" $ do
			tokenize "{\t}" `shouldBe` [OpenBrace, CloseBrace]

		it "should parse return statement" $ do
			tokenize "{ return }" `shouldBe` [OpenBrace, Return, CloseBrace]

		it "should parse semi colons" $ do
			tokenize ";;" `shouldBe` [SemiColon, SemiColon]

		it "should parse identifiers" $ do
			tokenize "function example()" `shouldBe` [Function, Identifier "example", OpenParens, CloseParens]
		
		it "should be able to end in an identifier" $ do
			tokenize "var example" `shouldBe` [Variable, Identifier "example"]

		it "should parse literal numbers as identifiers" $ do
			tokenize "function constant() { return 10; }" `shouldBe` [Function, Identifier "constant", OpenParens, CloseParens, OpenBrace,
																	  Return, Identifier "10", SemiColon, CloseBrace]