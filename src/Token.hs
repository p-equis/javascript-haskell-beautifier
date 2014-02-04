module Token (
	Token(..),
	toToken
) where

import Data.Map
import Prelude hiding (lookup)

data Token = Function 
			| OpenParens 
			| CloseParens 
			| OpenBrace 
			| CloseBrace 
			| Return
			| SemiColon
			deriving (Eq, Show)

toToken :: String -> Maybe Token
toToken x = lookup x tokens

tokens :: (Map String Token)
tokens = fromList([("function", Function), 
	("return", Return),
	(";", SemiColon),
	("(", OpenParens),
	(")", CloseParens),
	("{", OpenBrace),
	("}", CloseBrace)])