{-# OPTIONS -Wall #-}
module Main where

import Test.QuickCheck
import Data.CircularList

-- Make sure converting to/from lists works.
prop_list :: CList Int -> Bool
prop_list c = c == (fromList . toList $ c)

prop_focus :: CList Int -> Int -> Bool
prop_focus c v = v == (focus $ insertR v c)

prop_rot :: CList Int -> Bool
prop_rot c = c == (rotR $ rotL c)

prop_packL :: CList Int -> Bool
prop_packL c = c == (packL c)

prop_packR :: CList Int -> Bool
prop_packR c = c == (packR c)

prop_size :: [Int] -> Bool
prop_size [] = True -- Otherwise, _|_
prop_size l = (length l) == (size . fromList $ l)

prop_remL :: CList Int -> Bool
prop_remL cl | 1 == size cl = True -- Otherwise, _|_
             | otherwise = let f = focus cl
                           in cl == (insertL f $ removeL cl)

prop_remR :: CList Int -> Bool
prop_remR cl | 1 == size cl = True -- Otherwise, _|_
             | otherwise = let f = focus cl
                           in cl == (insertR f $ removeR cl)

main :: IO ()
main = do
    putStrLn "prop_list"
    quickCheck prop_list
    
    putStrLn "prop_rot"
    quickCheck prop_rot

    putStrLn "prop_focus"
    quickCheck prop_focus

    putStrLn "prop_packL"
    quickCheck prop_packL

    putStrLn "prop_packR"
    quickCheck prop_packR

    putStrLn "prop_size"
    quickCheck prop_size

    putStrLn "prop_remL"
    quickCheck prop_remL

    putStrLn "prop_remR"
    quickCheck prop_remR
