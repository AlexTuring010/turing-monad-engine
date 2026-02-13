module Parser where

import Types     -- This gives us access to our data types like Machine, Action, etc.
import qualified Map as MyMap -- This gives us the AVL tree implementation for our machine's state
import Tokenizer -- Give us access to the tokenizer and the isContained function
import Data.Char (isAlphaNum)

-- Project specifications recommend that we use Either for error messages
type ParseResult a = Either String a

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Some helper functions that I use in different parts of the parsing process --------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | This is a helper function to check if all elements in a list satisfy a predicate.
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all predicate (x:xs) = predicate x && all predicate xs

-- | This is a helper function to make it easier to work with Either in our parser.
-- It allows us to chain parsing steps together without deeply nested case statements.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x 
either _ g (Right y) = g y

-- | This is a helper function to make it easier to work with Maybe in our parser.
-- It allows us to chain parsing steps together without deeply nested case statements.
maybe :: b -> (a -> b) -> Maybe a -> b
maybe fallback _ Nothing = fallback
maybe _ f (Just x) = f x

-- I know that either and maybe are already defined in the Prelude, but I wanted to define 
-- them here so next time I read the code, I won't have to remember how they work or look them up.

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for the parsing of the Alphabet at the start of the program file --------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

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
parseSymbolMap (sym:rest) = do
    alphabetMap <- insertSymbol Empty sym
    parseSymbolMapTail alphabetMap rest
parseSymbolMap _ = Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"

parseSymbolMapTail :: AlphabetMap -> [String] -> ParseResult (AlphabetMap, [String])
parseSymbolMapTail alphabetMap ("}":rest) = Right (alphabetMap, rest)
parseSymbolMapTail alphabetMap (",":sym:more) = do
    updatedMap <- insertSymbol alphabetMap sym
    parseSymbolMapTail updatedMap more
parseSymbolMapTail _ (",":_) = Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"
parseSymbolMapTail _ _ = Left "Error: Invalid alphabet list (expected symbols separated by commas, ending with '}')"

insertSymbol :: AlphabetMap -> String -> ParseResult AlphabetMap
insertSymbol alphabetMap sym
    | not (isNameToken sym) = Left ("Error: Invalid alphabet symbol '" ++ sym ++ "' (must be alphanumeric)")
    | otherwise =
        maybe
            (Right (MyMap.insert sym () alphabetMap))
            (\_ -> Left ("Error: Duplicate alphabet symbol '" ++ sym ++ "'"))
            (MyMap.lookup sym alphabetMap)

isNameToken :: String -> Bool
isNameToken [] = False
isNameToken token = all isAlphaNum token

-- | Checks if a symbol is valid according to the defined alphabet.
-- This fulfills the requirement to check if written symbols belong to the alphabet.
isInAlphabet :: String -> AlphabetMap -> ParseResult String
isInAlphabet sym alphabetMap
    | sym == "_" = Right sym  -- "_" is the blank symbol
    | otherwise =
        maybe
            (Left ("Error: Symbol '" ++ sym ++ "' is not in the defined alphabet."))
            (\_ -> Right sym)
            (MyMap.lookup sym alphabetMap)

parseSymbolChar :: AlphabetMap -> String -> ParseResult Char
parseSymbolChar alphabetMap sym
    | sym == "_" = Right '_'
    | length sym /= 1 = Left ("Error: Symbol '" ++ sym ++ "' must be a single character")
    | otherwise = do
        _ <- isInAlphabet sym alphabetMap
        Right (head sym)

listContains :: Eq a => a -> [a] -> Bool
listContains _ [] = False
listContains target (x:xs)
    | target == x = True
    | otherwise = listContains target xs

ensureUniqueNames :: String -> [String] -> ParseResult [String]
ensureUniqueNames label names = go names Empty
  where
    go [] _ = Right names
    go (name:rest) nameMap =
        maybe
            (go rest (MyMap.insert name () nameMap))
            (\_ -> Left ("Error: Duplicate " ++ label ++ " '" ++ name ++ "'"))
            (MyMap.lookup name nameMap)

validateStateInList :: String -> [String] -> String -> ParseResult String
validateStateInList label states name
    | listContains name states = Right name
    | otherwise = Left ("Error: Unknown " ++ label ++ " state '" ++ name ++ "'")

validateStatesExist :: String -> [String] -> [String] -> ParseResult [String]
validateStatesExist label states names = go names []
  where
    go [] acc = Right (reverse acc)
    go (name:rest) acc =
                do
                        validated <- validateStateInList label states name
                        go rest (validated:acc)


{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- The main parseProgram function that we will export outside for Main.hs to use -----
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | The main entry point for the Parser (to be expanded Day 4-6).
-- Currently only handles the very first part of the grammar.
parseProgram :: String -> ParseResult Program
parseProgram input = do
    tokens <- tokenize input
    (alphabetMap, restTokens) <- parseAlphabet tokens
    (machines, leftoverTokens) <- parseMachineList alphabetMap restTokens
    case leftoverTokens of
        [] ->
            case MyMap.lookup "start" machines of
                Just _ ->
                    Right Program
                        { globalAlphabet = alphabetMap
                        , allMachines = machines
                        , startMachine = "start"
                        }
                Nothing -> Left "Error: Missing required machine named 'start'"
        _ -> Left "Error: Unexpected tokens after last machine"

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for parsing the machine list and individual machine definitions ----------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

parseMachineList :: AlphabetMap -> [String] -> ParseResult (MyMap String Machine, [String])
parseMachineList alphabetMap ("machine":_) = do
    (name, machine, rest) <- parseMachine alphabetMap
    parseMachineListTail alphabetMap (MyMap.insert name machine Empty) rest
parseMachineList _ _ = Left "Error: Expected at least one machine definition"

parseMachineListTail :: AlphabetMap -> MyMap String Machine -> [String] -> ParseResult (MyMap String Machine, [String])
parseMachineListTail alphabetMap machines ("machine":_) = do
    (name, machine, rest) <- parseMachine alphabetMap
    maybe
        (parseMachineListTail alphabetMap (MyMap.insert name machine machines) rest)
        (\_ -> Left ("Error: Duplicate machine name '" ++ name ++ "'"))
        (MyMap.lookup name machines)
parseMachineListTail _ machines tokens = Right (machines, tokens)

parseMachine :: AlphabetMap -> [String] -> ParseResult (String, Machine, [String])
parseMachine alphabetMap ("machine":name:"=":"{":rest)
    | not (isNameToken name) = Left ("Error: Invalid machine name '" ++ name ++ "'")
    | otherwise = do
        (states, restAfterStates) <- parseStatesSection rest
        (initState, restAfterInit) <- parseInitStateSection states restAfterStates
        (haltingStates, restAfterHalting) <- parseHaltingStatesSection states restAfterInit
        (transitions, restAfterFunction) <- parseFunctionSection alphabetMap states restAfterHalting
        parseMachineEnd name states initState haltingStates transitions restAfterFunction
parseMachine _ _ = Left "Error: Expected 'machine <name> = { ... }'"

parseMachineEnd :: String -> [String] -> String -> [String] -> MyMap (String, Char) (String, Action) -> [String] -> ParseResult (String, Machine, [String])
parseMachineEnd name states initState haltingStates transitions ("}":rest) =
    Right
        ( name
        , Machine
            { state = states
            , initial = initState
            , halting = haltingStates
            , transitions = transitions
            }
        , rest
        )
parseMachineEnd _ _ _ _ _ _ = Left "Error: Expected '}' to close machine definition"

parseStatesSection :: [String] -> ParseResult ([String], [String])
parseStatesSection ("states":"=":"{":rest) =
    do
        (states, restAfterStates) <- parseStateList rest
        uniqueStates <- ensureUniqueNames "state" states
        Right (uniqueStates, restAfterStates)
parseStatesSection _ = Left "Error: Expected 'states = { ... }'"

parseInitStateSection :: [String] -> [String] -> ParseResult (String, [String])
parseInitStateSection states ("init_state":"=":"{":name:"}":rest) =
    if isNameToken name
        then do
            validated <- validateStateInList "initial" states name
            Right (validated, rest)
        else Left ("Error: Invalid init state name '" ++ name ++ "'")
parseInitStateSection _ _ = Left "Error: Expected 'init_state = { <state> }'"

parseHaltingStatesSection :: [String] -> [String] -> ParseResult ([String], [String])
parseHaltingStatesSection states ("halting_states":"=":"{":rest) =
    do
        (haltStates, restAfterHalting) <- parseHaltingStateList rest
        validated <- validateStatesExist "halting" states haltStates
        Right (validated, restAfterHalting)
parseHaltingStatesSection _ _ = Left "Error: Expected 'halting_states = { ... }'"

parseStateList :: [String] -> ParseResult ([String], [String])
parseStateList ("}":_) = Left "Error: State list cannot be empty"
parseStateList (name:rest)
    | not (isNameToken name) = Left ("Error: Invalid state name '" ++ name ++ "'")
    | otherwise = parseStateListTail [name] rest
parseStateList _ = Left "Error: Invalid state list (expected names separated by commas, ending with '}')"

parseStateListTail :: [String] -> [String] -> ParseResult ([String], [String])
parseStateListTail acc ("}":rest) = Right (reverse acc, rest)
parseStateListTail acc (",":name:more)
    | not (isNameToken name) = Left ("Error: Invalid state name '" ++ name ++ "'")
    | otherwise = parseStateListTail (name:acc) more
parseStateListTail _ _ = Left "Error: Invalid state list (expected names separated by commas, ending with '}')"

parseHaltingStateList :: [String] -> ParseResult ([String], [String])
parseHaltingStateList ("}":rest) = Right ([], rest)
parseHaltingStateList tokens = parseStateList tokens

parseFunctionSection :: AlphabetMap -> [String] -> [String] -> ParseResult (MyMap (String, Char) (String, Action), [String])
parseFunctionSection alphabetMap states ("function":"=":"{":rest) =
    parseFunctionEntries alphabetMap states rest
parseFunctionSection _ _ _ = Left "Error: Expected 'function = { ... }'"

parseFunctionEntries :: AlphabetMap -> [String] -> [String] -> ParseResult (MyMap (String, Char) (String, Action), [String])
parseFunctionEntries alphabetMap states tokens = do
    (key, value, rest) <- parseFunctionEntry alphabetMap states tokens
    updated <- insertTransition Empty key value
    parseFunctionEntriesTail alphabetMap states updated rest

parseFunctionEntriesTail :: AlphabetMap -> [String] -> MyMap (String, Char) (String, Action) -> [String] -> ParseResult (MyMap (String, Char) (String, Action), [String])
parseFunctionEntriesTail alphabetMap states transitions (";":rest) =
    parseFunctionEntriesAfterSemicolon alphabetMap states transitions rest
parseFunctionEntriesTail _ _ _ _ = Left "Error: Expected ';' after function entry"

parseFunctionEntriesAfterSemicolon :: AlphabetMap -> [String] -> MyMap (String, Char) (String, Action) -> [String] -> ParseResult (MyMap (String, Char) (String, Action), [String])
parseFunctionEntriesAfterSemicolon _ _ transitions ("}":rest) = Right (transitions, rest)
parseFunctionEntriesAfterSemicolon alphabetMap states transitions tokens = do
    (key, value, rest) <- parseFunctionEntry alphabetMap states tokens
    updated <- insertTransition transitions key value
    parseFunctionEntriesTail alphabetMap states updated rest

parseFunctionEntry :: AlphabetMap -> [String] -> [String] -> ParseResult ((String, Char), (String, Action), [String])
parseFunctionEntry alphabetMap states (fromState:symbol:"->":toState:rest) = do
    validatedFrom <- validateStateInList "current" states fromState
    readChar <- parseSymbolChar alphabetMap symbol
    validatedTo <- validateStateInList "next" states toState
    (action, restAfterAction) <- parseAction alphabetMap rest
    Right ((validatedFrom, readChar), (validatedTo, action), restAfterAction)
parseFunctionEntry _ _ _ = Left "Error: Invalid function entry"

parseAction :: AlphabetMap -> [String] -> ParseResult (Action, [String])
parseAction alphabetMap tokens =
    case tokens of
        ("w":symbol:rest) -> do
            ch <- parseSymbolChar alphabetMap symbol
            Right (Write ch, rest)
        ("g":"left":rest) -> Right (Move MoveLeft, rest)
        ("g":"right":rest) -> Right (Move MoveRight, rest)
        ("c":name:rest)
            | isNameToken name -> Right (Call name, rest)
            | otherwise -> Left ("Error: Invalid machine name '" ++ name ++ "'")
        _ -> Left "Error: Invalid function result"

insertTransition :: MyMap (String, Char) (String, Action) -> (String, Char) -> (String, Action) -> ParseResult (MyMap (String, Char) (String, Action))
insertTransition transitions key value =
    maybe
        (Right (MyMap.insert key value transitions))
        (\_ -> Left "Error: Duplicate transition for state/symbol pair")
        (MyMap.lookup key transitions)

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Temporar testing functions that I use for testing while still under development ---
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | A small test helper for GHCi
testParser :: String -> IO ()
testParser input =
    either
        (\err -> putStrLn ("Failed: " ++ err))
        (\res -> putStrLn ("Success! Alphabet found: " ++ show res))
        (parseProgram input)