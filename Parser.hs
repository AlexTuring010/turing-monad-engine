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

ensureUniqueNames :: String -> [String] -> ParseResult [String]
ensureUniqueNames label names = go names Empty
  where
    go [] _ = Right names
    go (name:rest) nameMap =
        case MyMap.lookup name nameMap of
                Just _ -> Left ("Error: Duplicate " ++ label ++ " '" ++ name ++ "'")
                Nothing -> go rest (MyMap.insert name () nameMap)

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
    | sym == "_" = Left "Error: Alphabet cannot include '_' (blank symbol is implicit)"
    | not (isNameToken sym) = Left ("Error: Invalid alphabet symbol '" ++ sym ++ "' (must be alphanumeric or underscore)")
    | otherwise =
        case MyMap.lookup sym alphabetMap of
            Just _ -> Left ("Error: Duplicate alphabet symbol '" ++ sym ++ "'")
            Nothing -> Right (MyMap.insert sym () alphabetMap)

isNameToken :: String -> Bool
isNameToken [] = False
isNameToken token = all (\c -> isAlphaNum c || c == '_') token


-- | Checks if a symbol is valid according to the defined alphabet.
-- This fulfills the requirement to check if written symbols belong to the alphabet.
isInAlphabet :: String -> AlphabetMap -> ParseResult String
isInAlphabet sym alphabetMap
    | sym == "_" = Right sym  -- "_" is the blank symbol
    | otherwise =
        case MyMap.lookup sym alphabetMap of
            Just _ -> Right sym
            Nothing -> Left ("Error: Symbol '" ++ sym ++ "' is not in the defined alphabet.")

parseSymbolToken :: AlphabetMap -> String -> ParseResult String
parseSymbolToken alphabetMap sym
    | sym == "_" = Right sym
    | otherwise = do
        _ <- isInAlphabet sym alphabetMap
        Right sym

listContains :: Eq a => a -> [a] -> Bool
listContains _ [] = False
listContains target (x:xs)
    | target == x = True
    | otherwise = listContains target xs

{------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- Functions for extra semantic checks that we need to do after parsing, like checking ----
-- that all states have transitions for every symbol, that all called machines exist, etc -
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------}

-- | This function checks that all machines called by a machine's transitions actually exist in the program.
validateAllMachineCalls :: MyMap String Machine -> ParseResult ()
validateAllMachineCalls machines = go (MyMap.mapToList machines)
    where
        go [] = Right ()
        go ((name, machine):rest) = do
                validateMachineCalls machines name machine
                go rest

validateMachineCalls :: MyMap String Machine -> String -> Machine -> ParseResult ()
validateMachineCalls machines name machine = do
        validateCallsExist machines name (transitions machine)
        Right ()
    
validateCallsExist :: MyMap String Machine -> String -> MyMap (String, String) (String, Action) -> ParseResult ()
validateCallsExist machines machineName transitionsMap = go (MyMap.mapToList transitionsMap)
    where
        go [] = Right ()
        go ((_, (_, action)):rest) =
                case action of
                        Call target ->
                                case MyMap.lookup target machines of
                                        Just _ -> go rest
                                        Nothing -> Left ("Error: Machine '" ++ machineName ++ "' calls unknown machine '" ++ target ++ "'")
                        _ -> go rest

--------------------------------------------------------------------------------------------------------

-- | This function checks that a given state name is in the list of valid states for a machine.
validateStateInList :: [String] -> String -> Bool
validateStateInList states name = listContains name states

-- | This function checks that all state names in a list are in the list of valid states for a machine.
validateStatesExist :: [String] -> [String] -> Bool
validateStatesExist states names = all (validateStateInList states) names

-- | Completeness means that for every state in the machine, there is a defined transition for every symbol in the alphabet.
-- This is a requirement in the project specifications to ensure that the machine's behavior is fully defined.
validateCompleteness :: [String] -> String -> Machine -> ParseResult ()
validateCompleteness symbols machineName machine = goStates (state machine)
    where
        goStates [] = Right ()
        goStates (st:rest)
                | listContains st (halting machine) = goStates rest
                | otherwise = do
                        validateTransitionsForState symbols machineName st (transitions machine)
                        goStates rest

validateTransitionsForState :: [String] -> String -> String -> MyMap (String, String) (String, Action) -> ParseResult ()
validateTransitionsForState symbols machineName st transitionsMap = go symbols
    where
        go [] = Right ()
        go (sym:rest) =
            case MyMap.lookup (st, sym) transitionsMap of
                        Just _ -> go rest
                        Nothing ->
                                Left ("Error: Missing transition in machine '" ++ machineName ++ "' for state '" ++ st ++ "' and symbol '" ++ sym ++ "'")

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for parsing the machine definitions -------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

parseMachineList :: AlphabetMap -> [String] -> ParseResult (MyMap String Machine, [String])
parseMachineList alphabetMap tokens =
    case tokens of
        ("machine":_) -> do
            (name, machine, rest) <- parseMachine alphabetMap tokens
            parseMachineListTail alphabetMap (MyMap.insert name machine Empty) rest
        _ -> Left "Error: Expected at least one machine definition"

parseMachineListTail :: AlphabetMap -> MyMap String Machine -> [String] -> ParseResult (MyMap String Machine, [String])
parseMachineListTail alphabetMap machines tokens =
    case tokens of
        ("machine":_) -> do
            (name, machine, rest) <- parseMachine alphabetMap tokens
            case MyMap.lookup name machines of
                Just _ -> Left ("Error: Duplicate machine name '" ++ name ++ "'")
                Nothing -> parseMachineListTail alphabetMap (MyMap.insert name machine machines) rest
        _ -> Right (machines, tokens)

parseMachine :: AlphabetMap -> [String] -> ParseResult (String, Machine, [String])
parseMachine alphabetMap ("machine":name:"=":"{":rest)
    | not (isNameToken name) = Left ("Error: Invalid machine name '" ++ name ++ "'")
    | otherwise = do
        (states, restAfterStates) <- parseStatesSection name rest
        (initState, restAfterInit) <- parseInitStateSection name states restAfterStates
        (haltingStates, restAfterHalting) <- parseHaltingStatesSection name states restAfterInit
        (transitions, restAfterFunction) <- parseFunctionSection name alphabetMap states haltingStates restAfterHalting
        (machineName, machine, remainingTokens) <- parseMachineEnd name states initState haltingStates transitions restAfterFunction
        let allSymbols = "_" : MyMap.mapKeys alphabetMap
        validateCompleteness allSymbols machineName machine
        Right (machineName, machine, remainingTokens)
parseMachine _ _ = Left "Error: Expected 'machine <name> = { ... }'"

parseMachineEnd :: String -> [String] -> String -> [String] -> MyMap (String, String) (String, Action) -> [String] -> ParseResult (String, Machine, [String])
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
parseMachineEnd name _ _ _ _ _ = Left ("Error: Expected '}' to close machine definition for machine '" ++ name ++ "'")

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for parsing state lists -------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

parseStatesSection :: String -> [String] -> ParseResult ([String], [String])
parseStatesSection machineName ("states":"=":"{":rest) =
    do
        (states, restAfterStates) <- parseStateList machineName rest
        uniqueStates <- ensureUniqueNames "state" states
        Right (uniqueStates, restAfterStates)
parseStatesSection machineName _ = Left ("Error: Expected 'states = { ... }' in machine '" ++ machineName ++ "'")

parseInitStateSection :: String -> [String] -> [String] -> ParseResult (String, [String])
parseInitStateSection machineName states ("init_state":"=":"{":name:"}":rest) =
    if isNameToken name
        then
            if validateStateInList states name
                then Right (name, rest)
                else Left ("Error: Unknown initial state '" ++ name ++ "' in machine '" ++ machineName ++ "' (must be one of the states defined in the 'states' section)")
        else Left ("Error: Invalid init state name '" ++ name ++ "' in machine '" ++ machineName ++ "'")
parseInitStateSection machineName _ _ = Left ("Error: Expected 'init_state = { <state> }' in machine '" ++ machineName ++ "'")

parseHaltingStatesSection :: String -> [String] -> [String] -> ParseResult ([String], [String])
parseHaltingStatesSection machineName states ("halting_states":"=":"{":rest) =
    do
        (haltStates, restAfterHalting) <- parseHaltingStateList machineName rest
        if validateStatesExist states haltStates
            then Right (haltStates, restAfterHalting)
            else Left ("Error: Unknown halting state in machine '" ++ machineName ++ "'")
parseHaltingStatesSection machineName _ _ = Left ("Error: Expected 'halting_states = { ... }' in machine '" ++ machineName ++ "'")

parseStateList :: String -> [String] -> ParseResult ([String], [String])
parseStateList machineName ("}":_) = Left ("Error: State list cannot be empty in machine '" ++ machineName ++ "'")
parseStateList machineName (name:rest)
    | not (isNameToken name) = Left ("Error: Invalid state name '" ++ name ++ "' in machine '" ++ machineName ++ "'")
    | otherwise = parseStateListTail machineName [name] rest
parseStateList machineName _ = Left ("Error: Invalid state list in machine '" ++ machineName ++ "' (expected names separated by commas, ending with '}')")

parseStateListTail :: String -> [String] -> [String] -> ParseResult ([String], [String])
parseStateListTail _ acc ("}":rest) = Right (reverse acc, rest)
parseStateListTail machineName acc (",":name:more)
    | not (isNameToken name) = Left ("Error: Invalid state name '" ++ name ++ "' in machine '" ++ machineName ++ "'")
    | otherwise = parseStateListTail machineName (name:acc) more
parseStateListTail machineName _ _ = Left ("Error: Invalid state list in machine '" ++ machineName ++ "' (expected names separated by commas, ending with '}')")

parseHaltingStateList :: String -> [String] -> ParseResult ([String], [String])
parseHaltingStateList _ ("}":rest) = Right ([], rest)
parseHaltingStateList machineName tokens = parseStateList machineName tokens


{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for parsing the function transitions ------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

parseFunctionSection :: String -> AlphabetMap -> [String] -> [String] -> [String] -> ParseResult (MyMap (String, String) (String, Action), [String])
parseFunctionSection machineName _ _ _ ("function":"=":"{":"}":_) =
    Left ("Error: Function should have at least one entry in machine '" ++ machineName ++ "'")
parseFunctionSection machineName alphabetMap states haltingStates ("function":"=":"{":rest) = do
    (key, value, remaining, entryText) <- parseFunctionEntry machineName alphabetMap states haltingStates rest
    updated <- insertTransition machineName entryText Empty key value
    parseFunctionEntriesTail machineName alphabetMap states haltingStates updated remaining
parseFunctionSection machineName _ _ _ _ =
    Left ("Error: Expected 'function = { ... }' in machine '" ++ machineName ++ "'")

parseFunctionEntriesTail :: String -> AlphabetMap -> [String] -> [String] -> MyMap (String, String) (String, Action) -> [String] -> ParseResult (MyMap (String, String) (String, Action), [String])
parseFunctionEntriesTail _ _ _ _ transitions ("}":rest) = Right (transitions, rest)
parseFunctionEntriesTail machineName alphabetMap states haltingStates transitions (";":rest) =
    case rest of
        ("}":remaining) -> Right (transitions, remaining)
        _ -> do
            (key, value, remaining, entryText) <- parseFunctionEntry machineName alphabetMap states haltingStates rest
            updated <- insertTransition machineName entryText transitions key value
            parseFunctionEntriesTail machineName alphabetMap states haltingStates updated remaining
parseFunctionEntriesTail _ _ _ _ _ _ = Left "Error: Expected ';' or '}' after function entry"

parseFunctionEntry :: String -> AlphabetMap -> [String] -> [String] -> [String] -> ParseResult ((String, String), (String, Action), [String], String)
parseFunctionEntry machineName alphabetMap states haltingStates (fromState:symbol:"->":toState:rest) =
    if not (validateStateInList states fromState)
        then Left ("Error: Unknown from-state '" ++ fromState ++ "' in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        else if listContains fromState haltingStates
            then Left ("Error: Halting state '" ++ fromState ++ "' in machine '" ++ machineName ++ "' cannot have transitions (entry: " ++ entryText ++ ")")
            else case parseSymbolToken alphabetMap symbol of
                Left err -> Left (err ++ " in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
                Right readSymbol ->
                    if not (validateStateInList states toState)
                        then Left ("Error: Unknown to-state '" ++ toState ++ "' in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
                        else case parseAction machineName entryText alphabetMap rest of
                            Left err -> Left err
                            Right (action, restAfterAction) ->
                                Right ((fromState, readSymbol), (toState, action), restAfterAction, entryText)
    where
        entryText = unwords ([fromState, symbol, "->", toState] ++ take 2 rest)
parseFunctionEntry machineName _ _ _ tokens =
    Left ("Error: Invalid function entry in machine '" ++ machineName ++ "' ('" ++ entryText ++ "')")
    where
        entryText = unwords (take 4 tokens)

parseAction :: String -> String -> AlphabetMap -> [String] -> ParseResult (Action, [String])
parseAction machineName entryText alphabetMap tokens =
    case tokens of
        ("w":symbol:rest) ->
            case parseSymbolToken alphabetMap symbol of
                Left err -> Left (err ++ " in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
                Right sym -> Right (Write sym, rest)
        ("w":[]) -> Left ("Error: Unexpected end of input while parsing write action in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        ("g":"left":rest) -> Right (Move MoveLeft, rest)
        ("g":"right":rest) -> Right (Move MoveRight, rest)
        ("g":dir:_) -> Left ("Error: Invalid move direction '" ++ dir ++ "' in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        ("g":[]) -> Left ("Error: Unexpected end of input while parsing move action in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        ("c":name:rest)
            | isNameToken name -> Right (Call name, rest)
            | otherwise -> Left ("Error: Invalid machine name '" ++ name ++ "' in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        ("c":[]) -> Left ("Error: Unexpected end of input while parsing call action in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        (cmd:_) -> Left ("Error: Unknown action '" ++ cmd ++ "' in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        [] -> Left ("Error: Unexpected end of input while parsing action in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")

insertTransition :: String -> String -> MyMap (String, String) (String, Action) -> (String, String) -> (String, Action) -> ParseResult (MyMap (String, String) (String, Action))
insertTransition machineName entryText transitions key value =
    case MyMap.lookup key transitions of
        Just _ -> Left ("Error: Duplicate transition for state/symbol pair in machine '" ++ machineName ++ "' (entry: " ++ entryText ++ ")")
        Nothing -> Right (MyMap.insert key value transitions)


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
                Just _ -> do
                    -- At the end of parsing, we also want to validate that all called machines exist
                    -- Done at the end because the order of machine definitions shouldn't matter
                    validateAllMachineCalls machines
                    Right Program
                        { globalAlphabet = alphabetMap
                        , allMachines = machines
                        , startMachine = "start"
                        }
                Nothing -> Left "Error: Missing required machine named 'start'"
        _ -> Left "Error: Unexpected tokens after last machine"


{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Temporar testing functions that I use for testing while still under development ---
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | A small test helper for GHCi
testParser :: FilePath -> IO ()
testParser filePath = do
    input <- readFile filePath
    case parseProgram input of
        Left err -> putStrLn ("Failed: " ++ err)
        Right res -> putStrLn ("Success! Program parsed: " ++ show res)