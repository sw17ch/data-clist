module Main where

import Data.Ring

len :: Int
len = 10^7

l :: [Integer]
l = [0..1 * 10^7]

bench_to_from = toList . fromList
bench_next lst = applyNTimes (fromList lst) next (len * 2) 

main :: IO ()
main = do
    print $ sum $ bench_to_from l

applyNTimes i f n = (iterate f i) !! n
