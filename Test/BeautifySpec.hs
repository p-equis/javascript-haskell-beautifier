module BeautifySpec (spec) where

import Test.Hspec
import Token
import Beautify

spec :: Spec
spec = do
	describe "beautify" $ do
		it "should insert a space between keyword function and open parens" $ do
			beautify [Function, OpenParens] `shouldBe` [Function, Identifier " ", OpenParens]

		it "should preserve all tokens" $ do
			beautify [Function, OpenParens, CloseParens] `shouldBe` [Function, Identifier " ", OpenParens, CloseParens]