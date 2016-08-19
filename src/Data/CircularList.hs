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

This yields 1 as our new focus. Rotating this table left would return 0 to
the focus position.

-}
module Data.CircularList (
    -- * Data Types
    CList,
    -- * Functions
    -- ** Creation of CLists
    empty, fromList, singleton,
    -- ** Update of CList
    update, reverseDirection,
    -- ** Converting CLists to Lists
    leftElements, rightElements, toList, toInfList,
    minRep, minRepN,
    -- ** Extraction and Accumulation
    focus, insertL, insertR,
    removeL, removeR,
    -- ** Manipulation of Focus
    allRotations, allRotationsN, rotR, rotL, rotN,
    rotNR, rotNL, rotateTo, findRotateTo,
    -- ** List-like functions
    filterR, filterL, foldrR, foldrL, foldlR, foldlL,
    -- ** Manipulation of Packing
    balance, packL, packR,
    -- ** Information
    isEmpty, size,
) where

import Control.Applicative hiding (empty)
import Prelude
import Data.List(find,unfoldr,foldl')
import Control.DeepSeq(NFData(..))
import Control.Monad(join,(>=>))
import qualified Data.Traversable as T
import qualified Data.Foldable as F

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

-- |Reverse the direction of rotation.
reverseDirection :: CList a -> CList a
reverseDirection Empty = Empty
reverseDirection (CList l f r) = CList r f l

{- Creating Lists -}

-- |Starting with the focus, go left and accumulate all
-- elements of the CList in a list.
leftElements :: CList a -> [a]
leftElements Empty = []
leftElements (CList l f r) = f : (l ++ reverse r)

-- |Starting with the focus, go right and accumulate all
-- elements of the CList in a list.
rightElements :: CList a -> [a]
rightElements Empty = []
rightElements (CList l f r) = f : (r ++ reverse l)

-- |Make a list from a CList.
toList :: CList a -> [a]
toList = rightElements

-- |Make a CList into an infinite list.
toInfList :: CList a -> [a]
toInfList = cycle . toList

-- |Find the minimal representative of a CList, allowing only multiples
-- of n rotations.
minRepN :: (Ord a) => Int -> CList a -> [a]
minRepN _ Empty = []
minRepN n cl = minimum . map toList $ concat [ls, [cl], rs]
  where
    ls = leftRotationsN n cl
    rs = rightRotationsN n cl

-- |Find the minimal representative of a CList.
minRep :: (Ord a) => CList a -> [a]
minRep = minRepN 1

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

-- |Return all possible rotations of the provided 'CList', allowing only
-- multiples of n rotations, and where the focus is the provided 'CList'.
-- Note that this only makes sense if n divides the length of the list.
allRotationsN :: Int -> CList a -> CList (CList a)
allRotationsN _ Empty = singleton Empty
allRotationsN n cl = CList ls cl rs
  where
    ls = leftRotationsN n cl
    rs = rightRotationsN n cl

-- |Return all possible rotations of the provided 'CList', where the
-- focus is the provided 'CList'.
allRotations :: CList a -> CList (CList a)
allRotations = allRotationsN 1

-- |Return all possible rotations of the provided 'CList', allowing only
-- multiples of n rotations, going to the left.
leftRotationsN :: Int -> CList a -> [CList a]
leftRotationsN n = unfoldr (fmap (join (,)) . applyN n mRotL)
  where
    applyN m = foldr1 (>=>) . replicate m

-- |Return all possible rotations of the provided 'CList', allowing only
-- multiples of n rotations, going to the right.
rightRotationsN :: Int -> CList a -> [CList a]
rightRotationsN n = unfoldr (fmap (join (,)) . applyN n mRotR)
  where
    applyN m = foldr1 (>=>) . replicate m

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

-- |Rotate the focus the specified number of times; if the index is
-- positive then it is rotated to the right; otherwise it is rotated
-- to the left.
rotN :: Int -> CList a -> CList a
rotN _ Empty = Empty
rotN _ cl@(CList [] _ []) = cl
rotN n cl = iterate rot cl !! n'
  where
    n' = abs n
    rot | n < 0     = rotL
        | otherwise = rotR

-- |A wrapper around 'rotN' that doesn't rotate the CList if @n <= 0@.
rotNR :: Int -> CList a -> CList a
rotNR n cl
  | n <= 0 = cl
  | otherwise = rotN n cl

-- |Rotate the focus the specified number of times to the left (but
-- don't rotate if @n <= 0@).
rotNL :: Int -> CList a -> CList a
rotNL n cl
  | n <= 0 = cl
  | otherwise = rotN (negate n) cl

-- |Rotate the 'CList' such that the specified element (if it exists)
-- is focused.
rotateTo :: (Eq a) => a -> CList a -> Maybe (CList a)
rotateTo a = findRotateTo (a==)

-- |Attempt to rotate the 'CList' such that focused element matches
-- the supplied predicate.
findRotateTo :: (a -> Bool) -> CList a -> Maybe (CList a)
findRotateTo p = find (maybe False p . focus) . toList . allRotations

{- List-like functions -}

-- |Remove those elements that do not satisfy the supplied predicate,
-- rotating to the right if the focus does not satisfy the predicate.
filterR :: (a -> Bool) -> CList a -> CList a
filterR = filterCL removeR

-- |As with 'filterR', but rotates to the /left/ if the focus does not
-- satisfy the predicate.
filterL :: (a -> Bool) -> CList a -> CList a
filterL = filterCL removeL

-- |Abstract away what to do with the focused element if it doesn't
-- match the predicate when filtering.
filterCL :: (CList a -> CList a) -> (a -> Bool) -> CList a -> CList a
filterCL _ _ Empty = Empty
filterCL rm p (CList l f r)
  | p f = cl'
  | otherwise = rm cl'
  where
    cl' = CList (filter p l) f (filter p r)

-- |A right-fold, rotating to the right through the CList.
foldrR :: (a -> b -> b) -> b -> CList a -> b
foldrR = foldrCL rightElements

-- |A right-fold, rotating to the left through the CList.
foldrL :: (a -> b -> b) -> b -> CList a -> b
foldrL = foldrCL leftElements

-- |Abstract away direction for a foldr.
foldrCL :: (CList a -> [a]) -> (a -> b -> b) -> b -> CList a -> b
foldrCL toL f a = foldr f a . toL

-- |A (strict) left-fold, rotating to the right through the CList.
foldlR :: (a -> b -> a) -> a -> CList b -> a
foldlR = foldlCL rightElements

-- |A (strict) left-fold, rotating to the left through the CList.
foldlL :: (a -> b -> a) -> a -> CList b -> a
foldlL = foldlCL leftElements

-- |Abstract away direction for a foldl'.
foldlCL :: (CList b -> [b]) -> (a -> b -> a) -> a -> CList b -> a
foldlCL toL f a = foldl' f a . toL

{- Manipulating Packing -}

-- |Balance the CList. Equivalent to `fromList . toList`
balance :: CList a -> CList a
balance = fromList . toList

-- |Move all elements to the left side of the CList.
packL :: CList a -> CList a
packL Empty = Empty
packL (CList l f r) = CList (l ++ reverse r) f []

-- |Move all elements to the right side of the CList.
packR :: CList a -> CList a
packR Empty = Empty
packR (CList l f r) = CList [] f (r ++ reverse l)

{- Information -}

-- |Returns true if the CList is empty.
isEmpty :: CList a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- |Return the size of the CList.
size :: CList a -> Int
size Empty = 0
size (CList l _ r) = 1 + length l + length r

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
  a == b = any ((toList a ==) . toList) . toList $ allRotations b

instance (Ord a) => Ord (CList a) where
  a `compare` b = minRep a `compare` minRep b

instance (NFData a) => NFData (CList a) where
  rnf Empty         = ()
  rnf (CList l f r) = rnf f
                      `seq` rnf l
                      `seq` rnf r

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
    fmap fn (CList l f r) = CList (fmap fn l) (fn f) (fmap fn r)

instance F.Foldable CList where
  foldMap = T.foldMapDefault

instance T.Traversable CList where
  -- | traverses the list from left to right, starting at the focus
  traverse _ Empty         = pure Empty
  traverse g (CList l f r) = (\f' r' l' -> CList l' f' r') <$> g f
                                                           <*> T.traverse g r
                                                           <*> T.traverse g l
