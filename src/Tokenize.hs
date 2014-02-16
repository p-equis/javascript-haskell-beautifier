module Tokenize (
	tokenize
) where

import Token

tokenize :: String -> [Token]
tokenize []        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('\"':xs) = tokenizeLiteralString xs $ Current ""
tokenize ('/':'/':xs) = tokenizeLineComment xs $ Current "//"
tokenize stream    = tokenize' stream $ Unfinished "" 

tokenize' :: String -> PartialToken -> [Token]
tokenize' xs     (Finished t)   = [t] ++ tokenize xs 
tokenize' (x:xs) (Unfinished s) = lookahead xs (Current s) $ Next $ partialToken [x]
tokenize' []     (Unfinished s) = [Identifier s]

lookahead :: String -> Current -> Next -> [Token]
lookahead xs (Current "") (Next (Finished t))   = [t] ++ tokenize xs
lookahead xs (Current s ) (Next (Finished t))   = [Identifier s, t] ++ tokenize xs
lookahead xs (Current s ) (Next (Unfinished " ")) = [Identifier s] ++ tokenize xs
lookahead xs (Current s ) (Next (Unfinished x)) = tokenize' xs $ partialToken $ s ++ x

tokenizeLiteralString :: String -> Current -> [Token]
tokenizeLiteralString [] (Current s) = [quote s]
tokenizeLiteralString ('\"':xs) (Current s) = [quote s] ++ tokenize xs
tokenizeLiteralString (x:xs)    (Current s) = tokenizeLiteralString xs $ Current $ s ++ [x]

quote :: String -> Token
quote s = Identifier $ "\"" ++ s ++ "\""

tokenizeLineComment :: String -> Current -> [Token]
tokenizeLineComment [] (Current s) = [Identifier s]
tokenizeLineComment ('\n':xs) (Current s) = [Identifier s] ++ tokenize xs
tokenizeLineComment (x:xs)    (Current s) = tokenizeLineComment xs $ Current $ s ++ [x]

data PartialToken = Finished Token | Unfinished String
data Current = Current String
data Next = Next PartialToken

partialToken :: String -> PartialToken
partialToken x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s
