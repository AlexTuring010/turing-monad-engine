module Map where
import Types

-- 1. We update the Node to store its height (an Int)
-- This helps us check if it's "heavy" on one side.
-- data MyMap k v = Empty | Node Int k v (MyMap k v) (MyMap k v)

-- Helper: Get height safely
getHeight :: MyMap k v -> Int
getHeight Empty = 0
getHeight (Node h _ _ _ _) = h

-- Helper: Calculate balance factor
balanceFactor :: MyMap k v -> Int
balanceFactor Empty = 0
balanceFactor (Node _ _ _ l r) = getHeight l - getHeight r

-- 2. The Rotation Functions (The "Surgery")
-- These just rearrange the Nodes to keep them level.
rotateRight :: MyMap k v -> MyMap k v
rotateRight (Node _ k v (Node _ lk lv ll lr) r) = 
    let newR = mkNode k v lr r
    in mkNode lk lv ll newR
rotateRight t = t

rotateLeft :: MyMap k v -> MyMap k v
rotateLeft (Node _ k v l (Node _ rk rv rl rr)) = 
    let newL = mkNode k v l rl
    in mkNode rk rv newL rr
rotateLeft t = t

-- 3. The Smart Constructor (Calculates height)
mkNode :: k -> v -> MyMap k v -> MyMap k v -> MyMap k v
mkNode k v l r = Node (1 + max (getHeight l) (getHeight r)) k v l r

-- 4. The Balanced Insert
insert :: (Ord k) => k -> v -> MyMap k v -> MyMap k v
insert newK newV Empty = Node 1 newK newV Empty Empty
insert newK newV (Node h k v l r)
    | newK < k  = rebalance (mkNode k v (insert newK newV l) r)
    | newK > k  = rebalance (mkNode k v l (insert newK newV r))
    | otherwise = Node h k v l r -- Key exists, do nothing

-- 5. The Rebalance Logic
-- This checks the 4 cases (Left-Left, Left-Right, etc.)
rebalance :: MyMap k v -> MyMap k v
rebalance t
    | balanceFactor t > 1 && balanceFactor (leftBranch t) >= 0 = rotateRight t
    | balanceFactor t > 1 = rotateRight (mkNode (key t) (val t) (rotateLeft (leftBranch t)) (rightBranch t))
    | balanceFactor t < -1 && balanceFactor (rightBranch t) <= 0 = rotateLeft t
    | balanceFactor t < -1 = rotateLeft (mkNode (key t) (val t) (leftBranch t) (rotateRight (rightBranch t)))
    | otherwise = t
  where 
    leftBranch (Node _ _ _ l _) = l
    rightBranch (Node _ _ _ _ r) = r
    key (Node _ k _ _ _) = k
    val (Node _ _ v _ _) = v