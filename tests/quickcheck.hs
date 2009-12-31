{-# OPTIONS -Wall #-}
module Main where

import Test.QuickCheck
import Data.Ring

prop_empty :: Bool
prop_empty = length (toList empty) == 0

prop_list :: [Int] -> Bool
prop_list l = l == (toList . fromList $ l)

-- left
-- right

prop_focus :: [Int] -> Bool
prop_focus l@(h:_) = (Just h) == (focus . fromList $ l)
prop_focus e = Nothing == (focus . fromList $ e) -- Empty List

prop_insert :: Int -> [Int] -> Bool
prop_insert v l = (v:l) == (toList . (flip insert v) . fromList $ l)

prop_remove :: [Int] -> Bool
prop_remove l@(_:xs) = xs == (toList . remove . fromList $ l)
prop_remove e = e == (toList . remove . fromList $ e) -- Empty List

prop_prev :: [Int] -> Bool
prop_prev l@(_:_) = (last l) : (init l) == (toList . prev . fromList $ l)
prop_prev e = e == (toList . prev . fromList $ e) -- Empty List

prop_next :: [Int] -> Bool
prop_next l@(h:ts) = ts ++ [h] == (toList . next . fromList $ l)
prop_next e = e == (toList . next . fromList $ e) -- Empty List

prop_balance :: [Int] -> Bool
prop_balance l = l == (toList . balance . fromList $ l)

prop_packL :: [Int] -> Bool
prop_packL l = l == (toList . packL . fromList $ l)

prop_packR :: [Int] -> Bool
prop_packR l = l == (toList . packR . fromList $ l)

prop_isEmpty :: [Int] -> Bool
prop_isEmpty [] = True == (isEmpty . fromList $ []) 
prop_isEmpty l  = False == (isEmpty . fromList $ l)

prop_size :: [Int] -> Bool
prop_size l = (length l) == (size . fromList $ l)

main :: IO ()
main = do
    quickCheck prop_empty
    quickCheck prop_list
    quickCheck prop_next
    quickCheck prop_prev
    quickCheck prop_focus
    quickCheck prop_insert
    quickCheck prop_remove
    quickCheck prop_balance
    quickCheck prop_packL
    quickCheck prop_packR
    quickCheck prop_isEmpty
    quickCheck prop_size
