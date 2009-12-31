{-# OPTIONS -Wall #-}
module Main where

import Test.QuickCheck
import Data.CircularList

prop_empty :: Bool
prop_empty = length (toList empty) == 0

prop_list :: [Int] -> Bool
prop_list l = l == (toList . fromList $ l)

prop_focus :: [Int] -> Bool
prop_focus l@(h:_) = (Just h) == (focus . fromList $ l)
prop_focus e = Nothing == (focus . fromList $ e) -- Empty List

prop_rotL :: [Int] -> Bool
prop_rotL l@(_:_) = (last l) : (init l) == (toList . rotL . fromList $ l)
prop_rotL e = e == (toList . rotL . fromList $ e) -- Empty List

prop_rotR :: [Int] -> Bool
prop_rotR l@(h:ts) = ts ++ [h] == (toList . rotR . fromList $ l)
prop_rotR e = e == (toList . rotR . fromList $ e) -- Empty List

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
    putStrLn "prop_empty"
    quickCheck prop_empty

    putStrLn "prop_list"
    quickCheck prop_list
    
    putStrLn "prop_rotR"
    quickCheck prop_rotR

    putStrLn "prop_rotL"
    quickCheck prop_rotL

    putStrLn "prop_focus"
    quickCheck prop_focus

    putStrLn "prop_balance"
    quickCheck prop_balance

    putStrLn "prop_packL"
    quickCheck prop_packL

    putStrLn "prop_packR"
    quickCheck prop_packR

    putStrLn "prop_isEmpty"
    quickCheck prop_isEmpty

    putStrLn "prop_size"
    quickCheck prop_size
