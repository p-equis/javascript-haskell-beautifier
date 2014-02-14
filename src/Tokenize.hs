module Tokenize (
	tokenize
) where

import Token

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize stream = tokenize' (Unfinished "") stream

tokenize' :: PartialToken -> String -> [Token]
tokenize' (Finished t) xs = [t] ++ tokenize xs 
tokenize' unfinished (x:xs) = lookahead unfinished xs $ accumulate [x]

lookahead :: PartialToken -> String -> PartialToken -> [Token]
lookahead (Unfinished "") xs (Finished t) = [t] ++ tokenize xs
lookahead (Unfinished s) xs (Finished t) = [Identifier s, t] ++ tokenize xs
lookahead (Unfinished s) xs (Unfinished x) = tokenize' (accumulate (s ++ x)) xs

data PartialToken = Finished Token | Unfinished String | Empty

accumulate :: String -> PartialToken
accumulate x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s
