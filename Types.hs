data Tape = Tape [Char] [Char]
-- Example: Tape ['b', 'a'] ['c', 'd'] 
-- represents a tape like ... a b [c] d ...

data Direction = MoveLeft | MoveRight

instance Eq Direction where
    MoveLeft == MoveLeft = True
    MoveRight == MoveRight = True
    _ == _ = False

instance Show Direction where
    show MoveLeft = "MoveLeft"
    show MoveRight = "MoveRight"

data Action = Write Char | Move Direction | Call String

instance Eq Action where
    (Write c1) == (Write c2) = c1 == c2
    (Move d1) == (Move d2) = d1 == d2
    (Call s1) == (Call s2) = s1 == s2
    _ == _ = False

instance Show Action where
    show (Write c) = "Write " ++ [c]
    show (Move d) = "Move " ++ show d
    show (Call s) = "Call " ++ s

-- Our map data structure will be a binary search tree so we can
-- have time complexity of O(log n) for lookups and insertions.
data MyMap k v = Empty | Node k v (MyMap k v) (MyMap k v)

-- We can only 'show' a Map if we know how to 'show' the keys and values.
instance (Show k, Show v) => Show (MyMap k v) where
    show Empty = "Empty"
    show (Node k v left right) =
        "(" ++ show  left ++ " <- [" ++ show k ++ ": " ++ show v ++ "] -> " ++ show right ++ ")"

data Machine = Machine {
    state :: [String],
    initial :: String,
    halting :: [String],
    transitions :: MyMap (String, Char) (String, Action)
}

instance Show Machine where
    show (Machine state initial halting transitions) =
        "Machine { state = " ++ show state ++
        ", initial = " ++ show initial ++
        ", halting = " ++ show halting ++
        ", transitions = " ++ show transitions ++
        " }"
    
data Program = Program {
    alphabet :: [Char],
    allMachines :: MyMap String Machine
}