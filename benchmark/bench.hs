module Main where

import Criterion
import Criterion.Main

import Data.CircularList

len :: Int
len = 10^7

l :: [Integer]
l = [0..1 * 10^7]

bench_to_from = toList . fromList
bench_rotR = applyNTimes (len * 2) rotR . fromList
bench_minRep = minRep . fromList

applyNTimes n f = foldr (.) id $ replicate n f

main :: IO ()
main = defaultMain
    [ bench "toList . fromList" $ nf bench_to_from l
    , bench "rotR"              $ nf bench_rotR    l
    , bench "minRep"            $ nf bench_minRep  l
    ]
