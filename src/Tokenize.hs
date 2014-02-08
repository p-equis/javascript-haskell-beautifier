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
tokenize stream = tokenize' emptyAccumulator stream

tokenize' :: Accumulator -> String -> [Token]
tokenize' (Finished token) stream = [token] ++ tokenize stream
tokenize' (Unfinished string) (x:xs) = tokenize' (accumulate (string ++ [x])) xs

data Accumulator = Finished Token | Unfinished String

emptyAccumulator :: Accumulator
emptyAccumulator = Unfinished ""

accumulate :: String -> Accumulator
accumulate x = case (toToken x) of
		 			Nothing -> Unfinished x
		 			(Just token) -> Finished token