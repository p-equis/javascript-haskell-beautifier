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

		it "should omit newline characters" $ do
			tokenize "{\n}" `shouldBe` [OpenBrace, CloseBrace]

		it "should parse return statement" $ do
			tokenize "{ return }" `shouldBe` [OpenBrace, Return, CloseBrace]

		it "should parse semi colons" $ do
			tokenize ";;" `shouldBe` [SemiColon, SemiColon]

		it "should parse identifiers" $ do
			tokenize "function example()" `shouldBe` [Function, Identifier "example", OpenParens, CloseParens]
		
		it "should be able to end in an identifier" $ do
			tokenize "var example" `shouldBe` [Variable, Identifier "example"]
	
		it "should not include trailing whitespace in an identifier" $ do
			tokenize "var example =" `shouldBe` [Variable, Identifier "example", Assignment]
		
		describe "strings" $ do
			
			it "should parse strings" $ do
				tokenize "var example=\"hey\"" `shouldBe` [Variable, Identifier "example", Assignment, StringLiteral "hey"]

			it "should allow whitespace in strings" $ do
				tokenize " \"hey there\" " `shouldBe` [StringLiteral "hey there"]

			it "should treat tokens inside quotes as identifiers, not as tokens" $ do
				tokenize "var example=\"{\"" `shouldBe` [Variable, Identifier "example", Assignment, StringLiteral "{"]

			it "should close an open string if it's the very end of the input" $ do
				tokenize "var example=\"{" `shouldBe` [Variable, Identifier "example", Assignment, StringLiteral "{"]

		describe "comments" $ do
			
			it "should treat single-line comments as identifiers" $ do
				tokenize "var x // this is x" `shouldBe` [Variable, Identifier "x", LineComment " this is x"]

			it "should end single-line comments with an end-of-line character" $ do
				tokenize "var x // hi\n;" `shouldBe` [Variable, Identifier "x", LineComment " hi", SemiColon]

			it "should understand block comments" $ do
				tokenize "var x; /* x\n\n */;" `shouldBe` [Variable, Identifier "x", SemiColon, BlockComment " x\n\n ", SemiColon]

		it "should parse literal numbers as identifiers" $ do
			tokenize "return 10;" `shouldBe` [Return, Identifier "10", SemiColon]


