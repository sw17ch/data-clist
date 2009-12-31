{- |
A simple purely functional ring data type.

Lets describe what we mean by 'ring'. A ring is a circular data structure
such that if you continue rotating the ring, you'll eventually return to
the element you first observed.

All of our analogies involve sitting at a table who's top surface rotates
about its center axis (think of those convenient rotating platforms one
often finds in an (Americanized) Chinese Restaurant).

Only the closest item on the table is avialable to us. In order to reach
other elements on the table, we need to rotate the table to the left or 
the right.

Our convention for this problem says that rotations to the right are a
forward motion while rotations to the left are backward motions.

We'll use the following ring for our examples:

>   8 7 6
>  9     5
> A       4
> B       3
>  C     2
>   D 0 1
>     ^

The pointer at the bottom represents our position at the table. The element
currently in front of is is referred to as the `focus`. So, in this case, our
focus is 0.

If we were to rotate the table to the right using the `rotR` operation, we'd
have the following table.

>   9 8 7
>  A     6
> B       5
> C       4
>  D     3
>   0 1 2
>     ^

This yeilds 1 as our new focus. Rotating this table left would return 0 to the
focus position.

-}
module Data.Ring (
    -- * Data Types
    Ring,
    -- * Functions
    -- ** Creation of Rings
    empty, fromList,
    -- ** Converting Rings to Lists
    leftElements, rightElements, toList, toInfList,
    -- ** Extraction and Accumulation
    focus, insertL, insertR,
    removeL, removeR,
    -- ** Manipulation of Focus (selected slot)
    rotR, rotL,
    -- ** Manipulation of Packing
    balance, packL, packR,
    -- ** Information
    isEmpty, size,
) where

-- | A functional ring type.
data Ring a = Empty
            | Ring a [a] [a] -- focus, left, right
    deriving (Eq)

-- | The show instance prints a tuple of the
-- balanced ring where the left list's right-most
-- element is the first element to the left. The
-- left most-most element of the right list is the
-- next element to the right.
instance (Show a) => Show (Ring a) where
    show ring = case balance ring of
                     (Ring f l r) -> show (reverse l,f,r)
                     Empty -> "Empty"

{- Creating Rings -}

-- | An empty ring.
empty :: Ring a
empty = Empty

-- |Make a ring from a list.
fromList :: [a] -> Ring a
fromList [] = Empty
fromList a@(i:is) = let len = length a
                        (r,l) = splitAt (len `div` 2) is
                    in Ring i (reverse l) r

{- Creating Lists -}

-- |Starting with the focus, go left and accumulate all
-- elements of the ring in a list.
leftElements :: Ring a -> [a]
leftElements Empty = []
leftElements (Ring f l r) = f : (l ++ (reverse r))

-- |Starting with the focus, go right and accumulate all
-- elements of the ring in a list.
rightElements :: Ring a -> [a]
rightElements Empty = []
rightElements (Ring f l r) = f : (r ++ (reverse l))

-- |Make a list from a ring.
toList :: Ring a -> [a]
toList = rightElements

-- |Make a ring into an infinite list.
toInfList :: Ring a -> [a]
toInfList = cycle . toList

{- Extraction and Accumulation -}

-- |Return the focus of the ring.
focus :: Ring a -> Maybe a
focus Empty = Nothing
focus (Ring f _ _) = Just f

-- |Insert an element into the ring as the new focus. The
-- old focus is now the next element to the right.
insertR :: Ring a -> a -> Ring a
insertR Empty i = Ring i [] []
insertR (Ring f l r) i = Ring i l (f:r)

-- |Insert an element into the ring as the new focus. The
-- old focus is now the next element to the left.
insertL :: Ring a -> a -> Ring a
insertL Empty i = Ring i [] []
insertL (Ring f l r) i = Ring i (f:l) r

-- |Remove the focus from the ring. The new focus is the
-- next element to the left.
removeL :: Ring a -> Ring a
removeL Empty = Empty
removeL (Ring _ [] []) = Empty
removeL (Ring _ (l:ls) rs) = Ring l ls rs
removeL (Ring _ [] rs) = let (f:ls) = reverse rs
                         in Ring f ls [] 

-- |Remove the focus from the ring.
removeR :: Ring a -> Ring a
removeR Empty = Empty
removeR (Ring _ [] []) = Empty
removeR (Ring _ l (r:rs)) = Ring r l rs
removeR (Ring _ l []) = let (f:rs) = reverse l
                        in Ring f [] rs

{- Manipulating Rotation -}

-- |Rotate the focus to the previous (left) element.
rotL :: Ring a -> Ring a
rotL Empty = Empty
rotL r@(Ring _ [] []) = r
rotL (Ring f (l:ls) rs) = Ring l ls (f:rs)
rotL (Ring f [] rs) = let (l:ls) = reverse rs
                      in Ring l ls [f]

-- |Rotate the focus to the next (right) element.
rotR :: Ring a -> Ring a
rotR Empty = Empty
rotR r@(Ring _ [] []) = r
rotR (Ring f ls (r:rs)) = Ring r (f:ls) rs
rotR (Ring f ls []) = let (r:rs) = reverse ls
                      in Ring r [f] rs

{- Manipulating Packing -}

-- |Balance the ring.
balance :: Ring a -> Ring a
balance = fromList . toList

-- |Move all elements to the left side of the ring.
packL :: Ring a -> Ring a
packL Empty = Empty
packL (Ring f l r) = Ring f (l ++ (reverse r)) []

-- |Move all elements to the right side of the ring.
packR :: Ring a -> Ring a
packR Empty = Empty
packR (Ring f l r) = Ring f [] (r ++ (reverse l))

{- Information -}

-- |Returns true if the ring is empty.
isEmpty :: Ring a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- |Return the size of the ring.
size :: Ring a -> Int
size Empty = 0
size (Ring _ l r) = 1 + (length l) + (length r)