module Tokenize (
	tokenize
) where

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Map
import Token

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize stream = tokenize' "" stream

tokenize' :: String -> String -> [Token]
tokenize' accumulator [] = 
	case (toToken accumulator) of 
			Nothing -> []
			(Just token) -> [token]
tokenize' accumulator stream@(x:xs) = 
	case (toToken accumulator) of 
			Nothing ->  tokenize' (accumulator ++ [x]) xs
			(Just token) -> [token] ++ (tokenize stream)

