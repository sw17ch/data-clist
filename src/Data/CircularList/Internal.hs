module Data.CircularList.Internal where

import Control.Applicative hiding (empty)
import Prelude
import Data.List(find,unfoldr,foldl')
import Control.DeepSeq(NFData(..))
import Control.Monad(join)
import qualified Data.Traversable as T
import qualified Data.Foldable as F

-- | A functional ring type.
data CList a = Empty
             | CList [a] a [a] Int

{- Creating CLists -}

-- | An empty CList.
empty :: CList a
empty = Empty

-- |Make a (balanced) CList from a list.
fromList :: [a] -> CList a
fromList [] = Empty
fromList a@(i:is) = let len = length a
                        (r,l) = splitAt (len `div` 2) is
                    in CList (reverse l) i r len

singleton :: a -> CList a
singleton a = CList [] a [] 1

{- Updating of CLists -}

-- |Replaces the current focus with a new focus.
update :: a -> CList a -> CList a
update v Empty = CList [] v [] 1
update v (CList l _ r s) = CList l v r s

-- |Reverse the direction of rotation.
reverseDirection :: CList a -> CList a
reverseDirection Empty = Empty
reverseDirection (CList l f r s) = CList r f l s

{- Creating Lists -}

-- |Starting with the focus, go left and accumulate all
-- elements of the CList in a list.
leftElements :: CList a -> [a]
leftElements Empty = []
leftElements (CList l f r _) = f : (l ++ (reverse r))

-- |Starting with the focus, go right and accumulate all
-- elements of the CList in a list.
rightElements :: CList a -> [a]
rightElements Empty = []
rightElements (CList l f r _) = f : (r ++ (reverse l))

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
focus (CList _ f _ _) = Just f

-- |Insert an element into the CList as the new focus. The
-- old focus is now the next element to the right.
insertR :: a -> CList a -> CList a
insertR i Empty = CList [] i [] 1
insertR i (CList l f r s) = CList l i (f:r) (s+1)

-- |Insert an element into the CList as the new focus. The
-- old focus is now the next element to the left.
insertL :: a -> CList a -> CList a
insertL i Empty = CList [] i [] 1
insertL i (CList l f r s) = CList (f:l) i r (s+1)

-- |Remove the focus from the CList. The new focus is the
-- next element to the left.
removeL :: CList a -> CList a
removeL Empty = Empty
removeL (CList [] _ [] _) = Empty
removeL (CList (l:ls) _ rs s) = CList ls l rs (s-1)
removeL (CList [] _ rs s) = let (f:ls) = reverse rs
                          in CList ls f [] (s-1)

-- |Remove the focus from the CList.
removeR :: CList a -> CList a
removeR Empty = Empty
removeR (CList [] _ [] _) = Empty
removeR (CList l _ (r:rs) s) = CList l r rs (s-1)
removeR (CList l _ [] s) = let (f:rs) = reverse l
                         in CList [] f rs (s-1)

{- Manipulating Rotation -}

-- |Return all possible rotations of the provided 'CList', where the
-- focus is the provided 'CList'.
allRotations :: CList a -> CList (CList a)
allRotations Empty = singleton Empty
allRotations cl@(CList _ _ _ s) = CList ls cl rs s
  where
    ls = unfoldr (fmap (join (,)) . mRotL) cl
    rs = unfoldr (fmap (join (,)) . mRotR) cl

-- |Rotate the focus to the previous (left) element.
rotL :: CList a -> CList a
rotL Empty = Empty
rotL r@(CList [] _ [] _) = r
rotL (CList (l:ls) f rs s) = CList ls l (f:rs) s
rotL (CList [] f rs s) = let (l:ls) = reverse rs
                       in CList ls l [f] s

-- |A non-cyclic version of 'rotL'; that is, only rotate the focus if
-- there is a previous (left) element to rotate to.
mRotL :: CList a -> Maybe (CList a)
mRotL (CList (l:ls) f rs s) = Just $ CList ls l (f:rs) s
mRotL _ = Nothing

-- |Rotate the focus to the next (right) element.
rotR :: CList a -> CList a
rotR Empty = Empty
rotR r@(CList [] _ [] _) = r
rotR (CList ls f (r:rs) s) = CList (f:ls) r rs s
rotR (CList ls f [] s) = let (r:rs) = reverse ls
                       in CList [f] r rs s

-- |A non-cyclic version of 'rotL'; that is, only rotate the focus if
-- there is a previous (left) element to rotate to.
mRotR :: CList a -> Maybe (CList a)
mRotR (CList ls f (r:rs) s) = Just $ CList (f:ls) r rs s
mRotR _ = Nothing

-- |Rotate the focus the specified number of times; if the index is
-- positive then it is rotated to the right; otherwise it is rotated
-- to the left.
rotN :: Int -> CList a -> CList a
rotN _ Empty = Empty
rotN _ cl@(CList [] _ [] _) = cl
rotN n cl@(CList _ _ _ s) = iterate rot cl !! n''
  where
    n' = n `mod` s 
    (n'', rot) = if n' >= s `div` 2 then (abs (n' - s), rotL) else (n', rotR)

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
filterCL rm p (CList l f r _)
  | p f = cl'
  | otherwise = rm cl'
  where
    l' = filter p l
    r' = filter p r
    cl' = CList l' f r' (length l' + 1 + length r')

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
packL (CList l f r s) = CList (l ++ (reverse r)) f [] s

-- |Move all elements to the right side of the CList.
packR :: CList a -> CList a
packR Empty = Empty
packR (CList l f r s) = CList [] f (r ++ (reverse l)) s

{- Information -}

-- |Returns true if the CList is empty.
isEmpty :: CList a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- |Return the size of the CList.
size :: CList a -> Int
size Empty = 0
size (CList _ _ _ s) = s

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

instance (NFData a) => NFData (CList a) where
  rnf Empty         = ()
  rnf (CList l f r _) = rnf f
                      `seq` rnf l
                      `seq` rnf r

instance Functor CList where
    fmap _ Empty = Empty
    fmap fn (CList l f r s) = CList (fmap fn l) (fn f) (fmap fn r) s

instance F.Foldable CList where
  foldMap = T.foldMapDefault

instance T.Traversable CList where
  -- | traverses the list from left to right, starting at the focus
  traverse _ Empty         = pure Empty
  traverse g (CList l f r s) = (\f' r' l' -> CList l' f' r' s) <$> g f
                                                           <*> T.traverse g r
                                                           <*> T.traverse g l
