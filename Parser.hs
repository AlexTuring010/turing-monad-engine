module Parser where

import Types     -- This gives us access to our data types like Machine, Action, etc.
import Map       -- This gives us the AVL tree implementation for our machine's state
import Tokenizer -- Give us access to the tokenizer and the isContained function

-- Project specifications recommend that we use Either for error messages
type ParseResult a = Either String a

-- | This is a helper function to make it easier to work with Either in our parser.
-- It allows us to chain parsing steps together without deeply nested case statements.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

-- | Extracts the alphabet list from the start of the program.
-- Grammar: alphabet = { character_list } 
parseAlphabet :: [String] -> ParseResult ([String], [String])
parseAlphabet ("alphabet":rest) = 
    let symbols = takeWhile (/= "machine") rest
        remaining = dropWhile (/= "machine") rest
    in if null symbols 
       then Left "Error: Alphabet cannot be empty"
       else Right (symbols, remaining)
parseAlphabet _ = Left "Error: Program must start with 'alphabet = { ... }'"

-- | Checks if a character is valid according to the defined alphabet.
-- This fulfills the requirement to check if written chars belong to the alphabet.
validateChar :: String -> [String] -> ParseResult String
validateChar c alphabet =
    if isContained c alphabet || c == "_"  -- "_" is the blank symbol 
    then Right c
    else Left ("Error: Character '" ++ c ++ "' is not in the defined alphabet.")

-- | The main entry point for the Parser (to be expanded Day 4-6).
-- Currently only handles the very first part of the grammar.
parseProgram :: String -> ParseResult [String]
parseProgram input =
    either
        Left
        (\tokens ->
            either
                Left
                (\(alphabet, _) ->
                    -- For today, we just return the alphabet to prove it works.
                    -- Tomorrow, we will pass the remaining tokens to the machine parser.
                    Right alphabet
                )
                (parseAlphabet tokens)
        )
        (tokenize input)

-- | A small test helper for GHCi
testParser :: String -> IO ()
testParser input = case parseProgram input of
    Left err -> putStrLn ("Failed: " ++ err)
    Right res -> putStrLn ("Success! Alphabet found: " ++ show res)