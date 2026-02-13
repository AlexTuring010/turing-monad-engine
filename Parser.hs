module Parser where

import Types     -- This gives us access to our data types like Machine, Action, etc.
import qualified Map as MyMap -- This gives us the AVL tree implementation for our machine's state
import Tokenizer -- Give us access to the tokenizer and the isContained function
import Data.Char (isAlphaNum)

-- Project specifications recommend that we use Either for error messages
type ParseResult a = Either String a

-- | This is a helper function to make it easier to work with Either in our parser.
-- It allows us to chain parsing steps together without deeply nested case statements.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x 
either _ g (Right y) = g y
-- I know that either is already defined in the Prelude, but I wanted to include it here
-- anyway so next time I read the code I don't have to remember what it does.

-- | Extracts the alphabet list from the start of the program.
-- Grammar: alphabet = { character_list } 
parseAlphabet :: [String] -> ParseResult (AlphabetMap, [String])
parseAlphabet ("alphabet":"=":"{":rest) =
    parseSymbolMap rest
parseAlphabet _ = Left "Error: Expected 'alphabet = { ... }'"

-- | Parses a comma-separated list of symbols ending with '}'.
-- Returns the alphabet map and the remaining tokens after the closing brace.
parseSymbolMap :: [String] -> ParseResult (AlphabetMap, [String])
parseSymbolMap ("}":_) = Left "Error: Alphabet cannot be empty"
parseSymbolMap (sym:rest) =
    either
        Left
        (\alphabetMap -> parseSymbolMapTail alphabetMap rest)
        (insertSymbol Empty sym)
parseSymbolMap _ = Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"

parseSymbolMapTail :: AlphabetMap -> [String] -> ParseResult (AlphabetMap, [String])
parseSymbolMapTail alphabetMap ("}":rest) = Right (alphabetMap, rest)
parseSymbolMapTail alphabetMap (",":rest) =
    case rest of
        (sym:more) ->
            either
                Left
                (\updatedMap -> parseSymbolMapTail updatedMap more)
                (insertSymbol alphabetMap sym)
        _ -> Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"
parseSymbolMapTail _ _ = Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"

-- | Checks if a symbol is valid according to the defined alphabet.
-- This fulfills the requirement to check if written symbols belong to the alphabet.
isInAlphabet :: String -> AlphabetMap -> ParseResult String
isInAlphabet sym alphabetMap
    | sym == "_" = Right sym  -- "_" is the blank symbol
    | otherwise =
        case MyMap.lookup sym alphabetMap of
            Just _ -> Right sym
            Nothing -> Left ("Error: Symbol '" ++ sym ++ "' is not in the defined alphabet.")

insertSymbol :: AlphabetMap -> String -> ParseResult AlphabetMap
insertSymbol alphabetMap sym
    | not (isNameToken sym) = Left ("Error: Invalid alphabet symbol '" ++ sym ++ "' (must be alphanumeric)")
    | otherwise =
        case MyMap.lookup sym alphabetMap of
            Just _ -> Left ("Error: Duplicate alphabet symbol '" ++ sym ++ "'")
            Nothing -> Right (MyMap.insert sym () alphabetMap)

isNameToken :: String -> Bool
isNameToken [] = False
isNameToken token = all isAlphaNum token

-- | The main entry point for the Parser (to be expanded Day 4-6).
-- Currently only handles the very first part of the grammar.
parseProgram :: String -> ParseResult Program
parseProgram input =
    either
        Left
        (\tokens ->
            either
                Left
                (\(alphabetMap, _) ->
                    -- For today, we just return the alphabet to prove it works.
                    -- Tomorrow, we will pass the remaining tokens to the machine parser.
                    Right Program
                        { globalAlphabet = alphabetMap
                        , allMachines = Empty
                        , startMachine = ""
                        }
                )
                (parseAlphabet tokens)
        )
        (tokenize input)

-- | A small test helper for GHCi
testParser :: String -> IO ()
testParser input =
    either
        (\err -> putStrLn ("Failed: " ++ err))
        (\res -> putStrLn ("Success! Alphabet found: " ++ show res))
        (parseProgram input)