
{-  The Computer Language Benchmarks Game

    http://benchmarksgame.alioth.debian.org/

    by James Brock

    Naïve Haskell implementation of fannkuch-redux.

    fannkuchNaïve doesn't work because the permutation order of
    Data.List.permutations is different than the fannkuch-redux
    reference implementation, so the evenness assigned to each permutation
    is wrong and the checksum comes out wrong.
-}

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main(main) where

import System.Environment
import Data.List

main :: IO ()
main = do
    n <- readIO . head =<< getArgs

    let (maxFlipsCount, checkSum) = fannkuchNaïve n

    putStr $ unlines
        [ show checkSum
        , "Pfannkuchen(" ++ show n ++ ") = " ++ show maxFlipsCount
        ]

fannkuchNaïve
    :: Int
    -> (Int, Int) -- ^ (maximum flip count, checksum)
fannkuchNaïve 0 = (0,0)
fannkuchNaïve n = (maxFlipCountFinal, checkSumFinal)
  where

    (maxFlipCountFinal, checkSumFinal, _) = foldl' fkAccum (0, 0, 0) $ permutations [1..n]

    fkAccum :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
    fkAccum (!maxFlipCount, !checkSum, !permIndex) xs = (maxFlipCount', checkSum', permIndex')
      where
        !fcnt          = flipsCount xs
        !maxFlipCount' = max fcnt maxFlipCount
        !checkSum'     = checkSum + if even permIndex then fcnt else -fcnt
        !permIndex'    = permIndex + 1

    flipsCount :: [Int] -> Int
    flipsCount xs0 = go xs0 0
      where
        go [] cnt = cnt
        go (1:_) !cnt = cnt
        go xs@(x:_) !cnt = go (reverse lxs ++ rxs) (cnt + 1)
          where
            (lxs, rxs) = splitAt x xs

