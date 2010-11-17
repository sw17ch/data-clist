{- |
A simple purely functional circular list, or ring, data type.

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

We'll use the following circular list for our examples:

>   8 7 6
>  9     5
> A       4
> B       3
>  C     2
>   D 0 1
>     ^

The pointer at the bottom represents our position at the table. The element
currently in front of is is referred to as the `focus`. So, in this case,
our focus is 0.

If we were to rotate the table to the right using the `rotR` operation, we'd
have the following table.

>   9 8 7
>  A     6
> B       5
> C       4
>  D     3
>   0 1 2
>     ^

This yeilds 1 as our new focus. Rotating this table left would return 0 to
the focus position.

-}
module Data.CircularList (
    -- * Data Types
    CList,
    -- * Functions
    -- ** Creation of CLists
    empty, fromList, singleton,
    -- ** Update of CList
    update,
    -- ** Converting CLists to Lists
    leftElements, rightElements, toList, toInfList,
    -- ** Extraction and Accumulation
    focus, insertL, insertR,
    removeL, removeR,
    -- ** Manipulation of Focus
    allRotations, rotR, rotL,
    -- ** Manipulation of Packing
    balance, packL, packR,
    -- ** Information
    isEmpty, size,
) where

import Data.List(inits,tails,unfoldr)
import Control.Monad(join)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | A functional ring type.
data CList a = Empty
             | CList [a] a [a]

{- Creating CLists -}

-- | An empty CList.
empty :: CList a
empty = Empty

-- |Make a (balanced) CList from a list.
fromList :: [a] -> CList a
fromList [] = Empty
fromList a@(i:is) = let len = length a
                        (r,l) = splitAt (len `div` 2) is
                    in CList (reverse l) i r

singleton :: a -> CList a
singleton a = CList [] a []

{- Updating of CLists -}

-- |Replaces the current focus with a new focus.
update :: a -> CList a -> CList a
update v Empty = CList [] v []
update v (CList l _ r) = CList l v r

{- Creating Lists -}

-- |Starting with the focus, go left and accumulate all
-- elements of the CList in a list.
leftElements :: CList a -> [a]
leftElements Empty = []
leftElements (CList l f r) = f : (l ++ (reverse r))

-- |Starting with the focus, go right and accumulate all
-- elements of the CList in a list.
rightElements :: CList a -> [a]
rightElements Empty = []
rightElements (CList l f r) = f : (r ++ (reverse l))

-- |Make a list from a CList.
toList :: CList a -> [a]
toList = rightElements

-- |Make a CList into an infinite list.
toInfList :: CList a -> [a]
toInfList = cycle . toList

{- Extraction and Accumulation -}

-- |Return the focus of the CList.
focus :: CList a -> Maybe a
focus Empty = Nothing
focus (CList _ f _) = Just f

-- |Insert an element into the CList as the new focus. The
-- old focus is now the next element to the right.
insertR :: a -> CList a -> CList a
insertR i Empty = CList [] i []
insertR i (CList l f r) = CList l i (f:r)

-- |Insert an element into the CList as the new focus. The
-- old focus is now the next element to the left.
insertL :: a -> CList a -> CList a
insertL i Empty = CList [] i []
insertL i (CList l f r) = CList (f:l) i r

-- |Remove the focus from the CList. The new focus is the
-- next element to the left.
removeL :: CList a -> CList a
removeL Empty = Empty
removeL (CList [] _ []) = Empty
removeL (CList (l:ls) _ rs) = CList ls l rs
removeL (CList [] _ rs) = let (f:ls) = reverse rs
                          in CList ls f []

-- |Remove the focus from the CList.
removeR :: CList a -> CList a
removeR Empty = Empty
removeR (CList [] _ []) = Empty
removeR (CList l _ (r:rs)) = CList l r rs
removeR (CList l _ []) = let (f:rs) = reverse l
                         in CList [] f rs

{- Manipulating Rotation -}

-- |Return all possible rotations of the provided 'CList', where the
-- focus is the provided 'CList'.
allRotations :: CList a -> CList (CList a)
allRotations Empty = singleton Empty
allRotations cl = CList ls cl rs
  where
    ls = unfoldr (fmap (join (,)) . mRotL) cl
    rs = unfoldr (fmap (join (,)) . mRotR) cl

-- |Rotate the focus to the previous (left) element.
rotL :: CList a -> CList a
rotL Empty = Empty
rotL r@(CList [] _ []) = r
rotL (CList (l:ls) f rs) = CList ls l (f:rs)
rotL (CList [] f rs) = let (l:ls) = reverse rs
                       in CList ls l [f]

-- |A non-cyclic version of 'rotL'; that is, only rotate the focus if
-- there is a previous (left) element to rotate to.
mRotL :: CList a -> Maybe (CList a)
mRotL (CList (l:ls) f rs) = Just $ CList ls l (f:rs)
mRotL _ = Nothing

-- |Rotate the focus to the next (right) element.
rotR :: CList a -> CList a
rotR Empty = Empty
rotR r@(CList [] _ []) = r
rotR (CList ls f (r:rs)) = CList (f:ls) r rs
rotR (CList ls f []) = let (r:rs) = reverse ls
                       in CList [f] r rs

-- |A non-cyclic version of 'rotL'; that is, only rotate the focus if
-- there is a previous (left) element to rotate to.
mRotR :: CList a -> Maybe (CList a)
mRotR (CList ls f (r:rs)) = Just $ CList (f:ls) r rs
mRotR _ = Nothing

{- Manipulating Packing -}

-- |Balance the CList. Equivalent to `fromList . toList`
balance :: CList a -> CList a
balance = fromList . toList

-- |Move all elements to the left side of the CList.
packL :: CList a -> CList a
packL Empty = Empty
packL (CList l f r) = CList (l ++ (reverse r)) f []

-- |Move all elements to the right side of the CList.
packR :: CList a -> CList a
packR Empty = Empty
packR (CList l f r) = CList [] f (r ++ (reverse l))

{- Information -}

-- |Returns true if the CList is empty.
isEmpty :: CList a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- |Return the size of the CList.
size :: CList a -> Int
size Empty = 0
size (CList l _ r) = 1 + (length l) + (length r)

{- Instances -}

instance (Show a) => Show (CList a) where
 showsPrec d cl  = showParen (d > 10) $
                   showString "fromList " . shows (toList cl)

instance (Read a) => Read (CList a) where
 readsPrec p = readParen (p > 10) $ \ r -> do
   ("fromList",s) <- lex r
   (xs,t) <- reads s
   return (fromList xs,t)

instance (Eq a) => Eq (CList a) where
  a == b = any (identical a) . toList $ allRotations b

-- |Determine if two 'CList's are structurally identical.
identical :: (Eq a) => CList a -> CList a -> Bool
identical Empty Empty = True
identical (CList ls1 f1 rs1) (CList ls2 f2 rs2) = f1 == f2
                                                  && ls1 == ls2
                                                  && rs1 == rs2
identical _ _ = False

instance Arbitrary a => Arbitrary (CList a) where
    arbitrary = frequency [(1, return Empty), (10, arbCList)]
        where arbCList = do
                l <- arbitrary
                f <- arbitrary
                r <- arbitrary
                return $ CList l f r
    shrink (CList l f r) = Empty : [ CList l' f' r' | l' <- shrink l,
                                                      f' <- shrink f,
                                                      r' <- shrink r]
    shrink Empty = []

instance Functor CList where
    fmap _ Empty = Empty
    fmap fn (CList l f r) = (CList (fmap fn l) (fn f) (fmap fn r))
