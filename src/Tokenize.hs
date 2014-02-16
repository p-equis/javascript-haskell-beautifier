module Tokenize (
	tokenize
) where

import Token

tokenize :: String -> [Token]
tokenize []        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\"':xs) = tokenizeLiteralString xs $ Current ""
tokenize stream    = tokenize' (Unfinished "") stream

tokenize' :: PartialToken -> String -> [Token]
tokenize' (Finished t) xs   = [t] ++ tokenize xs 
tokenize' (Unfinished s) (x:xs) = lookahead (Current s) xs $ Next (accumulate [x])
tokenize' (Unfinished s) [] = [Identifier s]

lookahead :: Current -> String -> Next -> [Token]
lookahead (Current "") xs (Next (Finished t))   = [t] ++ tokenize xs
lookahead (Current s)  xs (Next (Finished t))   = [Identifier s, t] ++ tokenize xs
lookahead (Current s)  xs (Next (Unfinished x)) = tokenize' (accumulate $ s ++ x) xs

tokenizeLiteralString :: String -> Current -> [Token]
tokenizeLiteralString ('\"':xs) (Current s) = [quote s] ++ tokenize xs
tokenizeLiteralString (x:xs) (Current s)    = tokenizeLiteralString xs $ Current $ s ++ [x]

quote :: String -> Token
quote s = Identifier $ "\"" ++ s ++ "\""

data PartialToken = Finished Token | Unfinished String
data Current = Current String
data Next = Next PartialToken

accumulate :: String -> PartialToken
accumulate x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s
