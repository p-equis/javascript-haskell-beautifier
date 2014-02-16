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
tokenize' unfinished (x:xs) = lookahead (Current unfinished) xs $ Next (accumulate [x])
tokenize' (Unfinished s) [] = [Identifier s]

lookahead :: Current -> String -> Next -> [Token]
lookahead (Current (Unfinished "")) xs (Next (Finished t)) = [t] ++ tokenize xs
lookahead (Current (Unfinished s)) xs (Next (Finished t)) = [Identifier s, t] ++ tokenize xs
lookahead (Current (Unfinished s)) xs (Next (Unfinished x)) = tokenize' (accumulate (s ++ x)) xs

data PartialToken = Finished Token | Unfinished String
data Current = Current PartialToken
data Next = Next PartialToken

accumulate :: String -> PartialToken
accumulate x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s
