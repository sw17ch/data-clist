-- | A simple purely functional ring data type.
module Data.Ring (
    -- * Data Types
    Ring,
    -- * Functions
    -- ** Creation of Rings
    empty, fromList,
    -- ** Converting Rings to Lists
    left, right, toList,
    -- ** Extraction and Accumulation
    focus, insert, remove,
    -- ** Manipulation of Focus (selected slot)
    next, prev,
    -- ** Manipulation of Packing
    balance, packL, packR,
    -- ** Information
    isEmpty, size,
) where

-- | A functional ring type.
data Ring a = Empty
            | Ring [a] a [a]
    deriving (Eq)

instance (Show a) => Show (Ring a) where
    show = show . toList 

{- Creating Rings -}

-- | An empty ring.
empty :: Ring a
empty = Empty

-- |Make a ring from a list.
fromList :: [a] -> Ring a
fromList [] = Empty
fromList a@(i:is) = let len = length a
                        (r,l) = splitAt (len `div` 2) is
                    in Ring (reverse l) i r

{- Creating Lists -}

-- |Starting with the focus, accumulate all the items in
-- the left direction.
left :: Ring a -> [a]
left Empty = []
left (Ring l f r) = f : (l ++ (reverse r))

-- |Starting with the focus, accumulate all the items in
-- the right direction.
right :: Ring a -> [a]
right Empty = []
right (Ring l f r) = f : (r ++ (reverse l))

-- |Make a list from a ring.
toList :: Ring a -> [a]
toList = right

{- Extraction and Accumulation -}

-- |Return the focus of the ring.
focus :: Ring a -> Maybe a
focus Empty = Nothing
focus (Ring _ f _) = Just f

-- |Insert an element into the ring.
insert :: Ring a -> a -> Ring a
insert Empty i = Ring [] i []
insert (Ring l f r) i = Ring l i (f:r)

-- |Remove the focus from the ring.
remove :: Ring a -> Ring a
remove Empty = Empty
remove (Ring [] _ []) = Empty
remove (Ring l _ (r:rs)) = Ring l r rs
remove (Ring l _ []) = let (f:rs) = reverse l
                       in Ring [] f rs

{- Manipulating Rotation -}

-- |Rotate the focus to the previous (left) element.
prev :: Ring a -> Ring a
prev Empty = Empty
prev r@(Ring [] _ []) = r
prev (Ring (l:ls) f rs) = Ring ls l (f:rs)
prev (Ring [] f rs) = let (l:ls) = reverse rs
                      in Ring ls l [f]

-- |Rotate the focus to the next (right) element.
next :: Ring a -> Ring a
next Empty = Empty
next r@(Ring [] _ []) = r
next (Ring ls f (r:rs)) = Ring (f:ls) r rs
next (Ring ls f []) = let (r:rs) = reverse ls
                      in Ring [f] r rs

{- Manipulating Packing -}

-- |Balance the ring.
balance :: Ring a -> Ring a
balance = fromList . toList

-- |Move all elements to the left side of the ring.
packL :: Ring a -> Ring a
packL Empty = Empty
packL (Ring l f r) = Ring (l ++ (reverse r)) f []

-- |Move all elements to the right side of the ring.
packR :: Ring a -> Ring a
packR Empty = Empty
packR (Ring l f r) = Ring [] f (r ++ (reverse l))

{- Information -}

-- |Returns true if the ring is empty.
isEmpty :: Ring a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- |Return the size of the ring.
size :: Ring a -> Int
size Empty = 0
size (Ring l _ r) = 1 + (length l) + (length r)
