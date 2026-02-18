module Types where

-- | Represents the tape of the Turing machine, with a focus on the current cell.
-- Example: Tape ["b", "a"] ["c", "d"]
-- represents a tape like ... a b [c] d ...
data Tape = Tape [String] [String]

instance Show Tape where
    show (Tape left right) =
        let current = case right of (c:_) -> c; [] -> "_"
            rightRest = case right of (_:rs) -> rs; [] -> []
        in "Tape { left: " ++ show (reverse left) ++ ", [" ++ current ++ "], right: " ++ show rightRest ++ " }"

-- | ExecutionContext tracks the state of machine execution
-- Includes the current machine, state, tape, and call stack for nested machine calls
data ExecutionContext = ExecutionContext
    { currentMachine :: String                    -- Which machine we're executing
    , currentState :: String                      -- Current state in that machine
    , tape :: Tape                                -- The tape
    , callStack :: [(String, String)]             -- Stack of (machine, returnState) for nested calls
    }

instance Show ExecutionContext where
    show ctx = "ExecutionContext { machine: " ++ currentMachine ctx ++
               ", state: " ++ currentState ctx ++
               ", tape: " ++ show (tape ctx) ++
               ", callStack: " ++ show (callStack ctx) ++ " }"

-- An action can be writing a symbol, moving the head, or calling another machine.
data Action = Write String | Move Direction | Call String

instance Eq Action where
    (Write s1) == (Write s2) = s1 == s2
    (Move d1) == (Move d2) = d1 == d2
    (Call s1) == (Call s2) = s1 == s2
    _ == _ = False

instance Show Action where
    show (Write s) = "Write " ++ s
    show (Move d) = "Move " ++ show d
    show (Call s) = "Call " ++ s

-- A g action moves the head left or right
data Direction = MoveLeft | MoveRight

instance Eq Direction where
    MoveLeft == MoveLeft = True
    MoveRight == MoveRight = True
    _ == _ = False

instance Show Direction where
    show MoveLeft = "MoveLeft"
    show MoveRight = "MoveRight"

-- Our map data structure will be an AVL tree so we can
-- have time complexity of O(log n) for lookups and insertions.
data MyMap k v = Empty | Node Int k v (MyMap k v) (MyMap k v)

-- We can only 'show' a Map if we know how to 'show' the keys and values.
instance (Show k, Show v) => Show (MyMap k v) where
    show Empty = "Empty"
    show (Node _ k v left right) =
        "(" ++ show  left ++ " <- [" ++ show k ++ ": " ++ show v ++ "] -> " ++ show right ++ ")"

-- A machine has a set of states, an initial state, a set of halting states, and a transition function.
data Machine = Machine {
    state :: [String],
    initial :: String,
    halting :: [String],
    transitions :: MyMap (String, String) (String, Action)
}

instance Show Machine where
    show (Machine state initial halting transitions) =
        "Machine { state = " ++ show state ++
        ", initial = " ++ show initial ++
        ", halting = " ++ show halting ++
        ", transitions = " ++ show transitions ++
        " }"

-- The global alphabet is a set of symbols that can be used on the tape.
type AlphabetMap = MyMap String ()

-- The program consists of a global alphabet, a set of machines, and a designated start machine.
-- The start machine will always be called "start" per spec, but we keep it as a field for flexibility.
data Program = Program {
    globalAlphabet :: AlphabetMap,
    allMachines :: MyMap String Machine,
    startMachine :: String
}

instance Show Program where
    show (Program globalAlphabet allMachines startMachine) =
        "Program { globalAlphabet = " ++ show globalAlphabet ++
        ", allMachines = " ++ show allMachines ++
        ", startMachine = " ++ show startMachine ++
        " }"