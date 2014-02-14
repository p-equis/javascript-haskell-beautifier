module Tokenize (
	tokenize
) where

import Token

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs
tokenize stream = removeEmptyIdentifiers $ tokenize' (Unfinished "") stream

removeEmptyIdentifiers :: [Token] -> [Token]
removeEmptyIdentifiers ts = filter (\x -> not $ isEmptyIdentifier x) ts

tokenize' :: PartialToken -> String -> [Token]
tokenize' (Finished token) stream = [token] ++ tokenize stream 
tokenize' (Unfinished string) (x:xs) = case (toToken [x]) of 
	(Just t) -> [Identifier string, t] ++ tokenize xs
	Nothing -> tokenize' (accumulate (string ++ [x])) xs

data PartialToken = Finished Token | Unfinished String | Empty

accumulate :: String -> PartialToken
accumulate x = fromMaybe (toToken x) x

fromMaybe :: Maybe Token -> String -> PartialToken
fromMaybe (Just t) _ = Finished t
fromMaybe Nothing s  = Unfinished s

isEmptyIdentifier :: Token -> Bool
isEmptyIdentifier (Identifier "") = True
isEmptyIdentifier _ = False