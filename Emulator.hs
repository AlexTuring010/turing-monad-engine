module Emulator where

import Types
import qualified Map as MyMap


{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for handling tape actions -----------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | Read the current cell on the tape
readCell :: Tape -> String
readCell (Tape _ (cell:_)) = cell
readCell (Tape _ []) = "_"  -- implicit blank

-- | Write a symbol to the current cell
writeCell :: Tape -> String -> Tape
writeCell (Tape left (_:right)) newCell = Tape left (newCell:right)
writeCell (Tape left []) newCell = Tape left [newCell]

-- | Move the head left
-- Per spec: if at the first cell, nothing happens (head stays in place)
moveLeft :: Tape -> Tape
moveLeft (Tape (l:ls) right) = Tape ls (l:right)
moveLeft tape = tape  -- Already at first cell, no movement

-- | Move the head right
moveRight :: Tape -> Tape
moveRight (Tape left (r:rs)) = Tape (r:left) (if null rs then ["_"] else rs)
moveRight (Tape left []) = Tape left ["_"]  -- Should never happen since we always ensure there's at least one cell

-- | Convert a list of symbols to a Tape with focus at the first cell
fromList :: [String] -> Tape
fromList symbols = Tape [] symbols

-- | Convert a Tape back to a list (flattening left and right)
toList :: Tape -> [String]
toList (Tape left right) = reverse left ++ right

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for handling execution context and call stack --------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | Push a machine call onto the stack, switching to the called machine
-- returnState is the state we'll be in when we return from the called machine
pushCall :: Program -> String -> String -> ExecutionContext -> Either String ExecutionContext
pushCall program calledMachine returnState ctx =
    case MyMap.lookup calledMachine (allMachines program) of
        Nothing -> 
            Left ("Error: Machine '" ++ calledMachine ++ "' not found")
        Just machine -> 
            Right ctx { callStack = (currentMachine ctx, returnState) : callStack ctx
                      , currentMachine = calledMachine
                      , currentState = initial machine
                      }
-- Note: the stack only contains the caller chain, not the current machine 
-- since the current machine is tracked separately in the context. 

-- | Pop a machine call from the stack, restoring the previous machine and state
popCall :: ExecutionContext -> Either String ExecutionContext
popCall ctx =
    case callStack ctx of
        [] -> Left "Error: Attempted to return from machine with empty call stack"
        ((prevMachine, returnState):rest) ->
            Right (ctx { callStack = rest
                       , currentMachine = prevMachine
                       , currentState = returnState
                       })

-- | Get the current machine from the program
-- Safe to use unsafe pattern matching because parser guarantees all referenced machines exist
getCurrentMachine :: Program -> ExecutionContext -> Machine
getCurrentMachine program ctx = machine
    where
        Just machine = MyMap.lookup (currentMachine ctx) (allMachines program)

{-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- Functions for the emulation of the recursive turing machine -----------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------}

-- | The main emulator function
-- Returns either an error or a tuple of (halting state, final tape contents)
-- Safe to unwrap machine lookup because parser ensures 'start' machine always exists
runEmulator :: Program -> [String] -> Either String (String, [String])
runEmulator program initialSymbols = emulationLoop program initialContext
    where
        tape = fromList initialSymbols
        Just startMachine = MyMap.lookup "start" (allMachines program)
        initialContext = ExecutionContext
                        { currentMachine = "start"
                        , currentState = initial startMachine
                        , tape = tape
                        , callStack = []
                        }

-- | Execute a Write action: update tape and state
executeWrite :: Program -> ExecutionContext -> String -> String -> Either String (String, [String])
executeWrite program ctx sym nextState = emulationLoop program newCtx
    where
        newTape = writeCell (tape ctx) sym
        newCtx = ctx { tape = newTape, currentState = nextState }

-- | Execute a Move action: update tape (respecting boundaries) and state
executeMove :: Program -> ExecutionContext -> Direction -> String -> Either String (String, [String])
executeMove program ctx dir nextState = emulationLoop program newCtx
  where
    newTape = case dir of
        MoveLeft -> moveLeft (tape ctx)
        MoveRight -> moveRight (tape ctx)
    newCtx = ctx { tape = newTape, currentState = nextState }

-- | Execute a Call action: push onto call stack and switch machines
executeCall :: Program -> ExecutionContext -> String -> String -> Either String (String, [String])
executeCall program ctx machName nextState =
    case pushCall program machName nextState ctx of
        Left err -> Left err
        Right newCtx -> emulationLoop program newCtx

-- | Check if in halting state; if so, either return or pop stack
-- Parser guarantees: halting states have no outgoing transitions, so we safely halt here
checkHalting :: Program -> ExecutionContext -> Either String (ExecutionContext, Maybe (String, [String]))
checkHalting program ctx =
    if currentState ctx `elem` halting currentMachineObj
        then case callStack ctx of
            [] -> Right (ctx, Just (currentState ctx, toList (tape ctx)))
            _ -> case popCall ctx of
                Left err -> Left err
                Right newCtx -> checkHalting program newCtx
    else Right (ctx, Nothing)  -- Not halting, continue with transition
    where
        currentMachineObj = getCurrentMachine program ctx
-- Above I initially had this tricky bug because I was not returning back the ctx, this is why we return it now
-- Its because when we reach a halting state, we may pop back to a caller, and that caller may also be in a halting 
-- state, and anyway, if we dont return the updated ctx, emulationLoop will still use the old one causing weird bugs 
-- like calling executeTransition for a halting state.

-- | Look up and execute a transition
-- Parser guarantees: all non-halting states have transitions for every symbol (completeness check)
executeTransition :: Program -> ExecutionContext -> Either String (String, [String])
executeTransition program ctx =
    case action of
        Write sym -> executeWrite program ctx sym nextState
        Move dir -> executeMove program ctx dir nextState
        Call machName -> executeCall program ctx machName nextState
    where
        currentMachineObj = getCurrentMachine program ctx
        symbol = readCell (tape ctx)
        key = (currentState ctx, symbol)
        Just (nextState, action) = MyMap.lookup key (transitions currentMachineObj)

-- | The main emulation loop - orchestrates termination checks and transitions
emulationLoop :: Program -> ExecutionContext -> Either String (String, [String])
emulationLoop program ctx =
    case checkHalting program ctx of
        Left err -> Left err
        Right (_, Just result) -> Right result  -- Halting, return result
        Right (updatedCtx, Nothing) -> executeTransition program updatedCtx  -- Not halting, proceed with transition using updated context