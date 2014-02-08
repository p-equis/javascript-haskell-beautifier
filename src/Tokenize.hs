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
tokenize stream = tokenize' emptyToken stream

tokenize' :: PartialToken -> String -> [Token]
tokenize' (Finished token) stream = [token] ++ tokenize stream
tokenize' (Unfinished string) (x:xs) = tokenize' (accumulate (string ++ [x])) xs

data PartialToken = Finished Token | Unfinished String

emptyToken :: PartialToken
emptyToken = Unfinished ""

accumulate :: String -> PartialToken
accumulate x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s