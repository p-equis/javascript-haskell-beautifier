module Beautify (beautify)
where

import Token

beautify :: [Token] -> [Token]
beautify [] = []
beautify (Function:OpenParens:xs) = [Function, Identifier " ", OpenParens] ++ beautify xs
beautify xs = xs