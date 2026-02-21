module Tokenizer (tokenize, isContained) where

import Data.Char (isAlphaNum)

-- Returns True if the character is in the string, False otherwise.
isContained :: Char -> String -> Bool
isContained _ [] = False
isContained c (x:xs)
    | c == x    = True
    | otherwise = isContained c xs

-- Helper function to split a list based on a predicate, 
-- returning the matched and remaining parts.
spanWhile :: (a -> Bool) -> [a] -> ([a], [a])
spanWhile _ [] = ([], [])
spanWhile p (x:xs)
        | p x = (x:ys, zs)
        | otherwise = ([], x:xs)
    where
        (ys, zs) = spanWhile p xs

-- Structural symbols defined in the grammar
isSyntaxChar :: Char -> Bool
isSyntaxChar c = isContained c "{},=;"

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

isSpaceChar :: Char -> Bool
isSpaceChar ch = ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'

-- Turns the raw string into a list of clean tokens
-- Keeps symbols and the "->" operator as separate tokens.
-- Comments starting with -- and extending to newline (or EOF) are ignored.
tokenize :: String -> Either String [String]
tokenize = go []
  where
    go acc [] = Right (reverse acc)
    go acc (c:cs)
        | isSpaceChar c = go acc cs
        | isSyntaxChar c = go ([c] : acc) cs
        | c == '-' = goDash acc cs
        | c == '>' = Left "Error: Unexpected '>' (use '->')"
        | isNameChar c = go (word : acc) rest
        | otherwise = Left ("Error: Invalid character '" ++ [c] ++ "' in input")
        where
            (word, rest) = spanWhile isNameChar (c:cs)
            goDash acc ('-':rest) = go acc (skipComment rest)  -- comment
            goDash acc ('>':rest) = go ("->" : acc) rest       -- arrow
            goDash _ _ = Left "Error: Unexpected '-' (use '->')"
            skipComment [] = []
            skipComment ('\n':xs) = xs
            skipComment (_:xs) = skipComment xs
-- The grammar does not define comments, however I added them because it makes my addition.turing and reverse.turing
-- programs much easier to read and write, its a simple extension that does not effect the core logic.