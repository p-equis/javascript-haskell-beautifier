module Tokenize (
	tokenize
) where

import Token

tokenize :: String -> [Token]
tokenize []        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('\"':xs) = tokenizeStringLiteral xs $ Current ""
tokenize ('/':'/':xs) = tokenizeLineComment xs $ Current "//"
tokenize ('/':'*':xs) = tokenizeBlockComment xs $ Current ""
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

tokenizeStringLiteral :: String -> Current -> [Token]
tokenizeStringLiteral [] (Current s) = [StringLiteral s]
tokenizeStringLiteral ('\"':xs) (Current s) = [StringLiteral s] ++ tokenize xs
tokenizeStringLiteral (x:xs)    (Current s) = tokenizeStringLiteral xs $ Current $ s ++ [x]

tokenizeLineComment :: String -> Current -> [Token]
tokenizeLineComment [] (Current s) = [Identifier s]
tokenizeLineComment ('\n':xs) (Current s) = [Identifier s] ++ tokenize xs
tokenizeLineComment (x:xs)    (Current s) = tokenizeLineComment xs $ Current $ s ++ [x]

tokenizeBlockComment :: String -> Current -> [Token]
tokenizeBlockComment [] (Current s) = [blockComment s]
tokenizeBlockComment ('*':'/':xs) (Current s) = [blockComment s] ++ tokenize xs
tokenizeBlockComment (x:xs)    (Current s) = tokenizeBlockComment xs $ Current $ s ++ [x]

blockComment :: String -> Token
blockComment s = Identifier $ "/*" ++ s ++ "*/"

data PartialToken = Finished Token | Unfinished String
data Current = Current String
data Next = Next PartialToken

partialToken :: String -> PartialToken
partialToken x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s
