module Tokenize where

import Data.List

data Token = Function | OpenParens | CloseParens | OpenBrace | CloseBrace deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize (' ':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize ('(':xs) = [OpenParens] ++ tokenize xs
tokenize (')':xs) = [CloseParens] ++ tokenize xs
tokenize ('{':xs) = [OpenBrace] ++ tokenize xs
tokenize ('}':xs) = [CloseBrace] ++ tokenize xs
tokenize stream  
	| "function" `isPrefixOf` stream = [Function] ++ (tokenize remainder)
	where remainder = drop (length "function") stream
tokenize _ = []
