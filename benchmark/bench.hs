module Main where

import Data.CircularList

len :: Int
len = 10^7

l :: [Integer]
l = [0..1 * 10^7]

bench_to_from = toList . fromList
bench_rotR lst = applyNTimes (fromList lst) rotR (len * 2) 

main :: IO ()
main = do
    print $ sum $ bench_to_from l

applyNTimes i f n = (iterate f i) !! n
