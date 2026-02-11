module Map where

import Types -- This allows us to use the MyMap definition from Types.hs

-- Insert function: Adds or updates a key-value pair
insert :: (Ord k) => k -> v -> MyMap k v -> MyMap k v
insert newKey newValue Empty = Node newKey newValue Empty Empty
insert newKey newValue (Node key value left right)
    | newKey == key = Node newKey newValue left right -- Update existing key
    | newKey < key  = Node key value (insert newKey newValue left) right -- Insert into left subtree
    | newKey > key  = Node key value left (insert newKey newValue right) -- Insert into right subtree

-- Find function: Retrieves the value associated with a key, if it exists
find :: (Ord k) => k -> MyMap k v -> Maybe v
find _ Empty = Nothing
find searchKey (Node key value left right)
    | searchKey == key = Just value -- Key found
    | searchKey < key  = find searchKey left -- Search in left subtree
    | searchKey > key  = find searchKey right -- Search in right subtree

