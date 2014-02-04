module Tokenize where

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Map

data Token = Function 
			| OpenParens 
			| CloseParens 
			| OpenBrace 
			| CloseBrace 
			| Return
			deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize stream = tokenize' "" stream

tokenize' :: String -> String -> [Token]
tokenize' accumulator [] = case (lookup accumulator tokens) of 
															Nothing -> []
															(Just token) -> [token]
tokenize' accumulator stream@(x:xs) = maybe keepAccumulating parseToken candidateToken
	where keepAccumulating = tokenize' (accumulator ++ [x]) xs
	      parseToken token = [token] ++ (tokenize stream)
	      candidateToken = lookup accumulator tokens

tokens :: (Map String Token)
tokens = fromList([("function", Function), 
	("return", Return),
	("(", OpenParens),
	(")", CloseParens),
	("{", OpenBrace),
	("}", CloseBrace)])