{-  The Computer Language Benchmarks Game

    http://benchmarksgame.alioth.debian.org/

    contributed by Branimir Maksimovic
    optimized/rewritten by Bryan O'Sullivan
    modified by Gabriel Gonzalez
    parallelized by James Brock
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
--- {-# LANGAUGE RecordWildCards #-}
--- {-# LANGAUGE NamedFieldPuns #-}

import System.Environment
import Text.Printf
import Data.Bits
import Data.Monoid
import Data.List

import Debug.Trace

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Unboxed as V

main = do
    n <- getArgs >>= readIO.head

    --- let (maxFlipCount, checkSum) = fannkuchNaïve n
    --- putStr $ unlines
    ---     [ show checkSum
    ---     , "Pfannkuchen(" ++ show n ++ ") = " ++ show maxFlipCount
    ---     ]

    (checksum,maxflips) <- fannkuch n
    printf "%d\nPfannkuchen(%d) = %d\n" checksum n maxflips

fannkuch :: Int -> IO (Int, Int)
fannkuch n = do

    -- VM.IOVector Int vectors of length n
    perm <- V.unsafeThaw $ V.enumFromN 1 n
    !tperm <- VM.new n
    !cnt <- VM.replicate n 0


    let
        loop :: Int -> Int -> Int -> IO(Int,Int)
        loop !c !m !pc = do
            b <- next_permutation perm n cnt
            b' <- V.freeze perm
            traceIO $ show $ V.toList b'
            if b == False
                then return (c,m)
                else do
                    VM.unsafeCopy tperm perm
                    let count_flips !flips = {-# SCC "count_flips" #-} do
                            f <- VM.unsafeRead tperm 0
                            if f == 1
                            then loop (c + (if pc .&. 1 == 0 then flips else -flips))
                                        (max m flips)
                                        (pc+1)
                            else do
                                VG.reverse $ VM.unsafeSlice 0 f tperm
                                count_flips (flips+1)
                    count_flips 0
    loop 0 0 1

next_permutation :: VM.IOVector Int -> Int -> VM.IOVector Int -> IO Bool
next_permutation perm !n !cnt = loop 1
    where
    loop :: Int -> IO Bool
    loop i
        | i >= n = done i
        | otherwise = do
            tmp <- VM.unsafeRead perm 0
            let
                rotate :: Int -> IO()
                rotate j
                    | j >= i = VM.unsafeWrite perm i tmp
                    | otherwise = do
                        !v <- VM.unsafeRead perm (j+1)
                        VM.unsafeWrite perm j v
                        rotate (j+1)
            rotate 0
            v <- VM.unsafeRead cnt i
            if v >= i
            then VM.unsafeWrite cnt i 0 >> loop (i+1)
            else done i

    done :: Int -> IO Bool
    done i
        | i >= n = return False
        | otherwise = do
            v <- VM.unsafeRead cnt i
            VM.unsafeWrite cnt i (v+1)
            return True

-- http://en.literateprograms.org/Kth_permutation_%28Haskell%29

-- radix representation
rr :: Int -> Int -> [Int]
rr 0 _ = []
rr n k = k `mod` n : rr (n-1) (k `div` n)
--- rr n k = rr (n-1) (k `div` n) <> [k `mod` n]

-- direct representation from radix rep
dfr :: [Int] -> [Int]
dfr = foldr (\x rs -> x : [r + (if x <= r then 1 else 0) | r <- rs]) []

-- generate permutation
perm :: [a] -> Int -> [a]
perm xs k = [xs !! i | i <- dfr (rr (length xs) k)]



-- this is the special permuation ordering necessary for the permutation indices to have the required evenness.
permute :: [Int] -> [[Int]]
permute [] = [[]]
permute [x] = [[x]]
permute xs = take (length xs) $ iterate rotateLeft xs
  where
    rotateLeft (x:xs) = xs ++ [x]
    -- subRotate (x:xs) = x : rotateLeft xs
    -- subRotate (x:xs) =


--- data FK = FK
---    { fkMaxFlipCount :: !Int
---    , fkCheckSum     :: !Int
---    , fkPermIndex    :: !Int
---    } deriving (Show)

fannkuchNaïve
    :: Int
    -> (Int, Int) -- ^ (maximum flip count, checksum)
fannkuchNaïve 0 = (0,0)
fannkuchNaïve n = (maxFlipCountFinal, checkSumFinal)
  where


    (maxFlipCountFinal, checkSumFinal, _) = foldl' fkAccum (0, 0, 0) $ permutations [1..n]

    fkAccum :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
    fkAccum (maxFlipCount, checkSum, permIndex) xs =
        trace (show permIndex ++ " " ++ show xs ++ " " ++ show fcnt ++ "  +" ++ show (if even permIndex then fcnt else negate fcnt) ++ "=" ++ show checkSum')
        (maxFlipCount', checkSum', permIndex')
      where
        fcnt          = flipsCount xs
        maxFlipCount' = max fcnt maxFlipCount
        checkSum'     = checkSum + if even permIndex then fcnt else -fcnt
        permIndex'    = permIndex + 1

    --- FK maxFlipCount checkSum _ = foldl' fkAccum (FK 0 0 0) $ permutations [1..n]

    --- fkAccum :: FK -> [Int] -> FK
    --- fkAccum fk xs =
    ---     --- trace xs $ trace
    ---     traceShow (fkPermIndex fk) $ traceShow xs $ traceShow fcnt $ traceShowId
    ---     FK
    ---     { fkMaxFlipCount = max fcnt $ fkMaxFlipCount fk
    ---     , fkCheckSum     = fkCheckSum fk + if even (fkPermIndex fk) then fcnt else negate fcnt
    ---     , fkPermIndex    = fkPermIndex fk + 1
    ---     }
    ---   where
    ---     fcnt = flipsCount xs

    flipsCount :: [Int] -> Int
    flipsCount xs0 = go xs0 0
      where
        go (1:_) !cnt = cnt
        go xs@(x:_) !cnt = go (reverse lxs ++ rxs) (cnt + 1)
          where
            (lxs, rxs) = splitAt x xs

        --- go xs cnt = go (flipFront xs) (cnt+1)
        ---   where
        ---     flipFront xs@(x:_) = reverse ls ++ rs
        ---       where
        ---         (ls, rs) = splitAt x xs


---                     let count_flips !flips = {-# SCC "count_flips" #-} do
---                             f <- VM.unsafeRead tperm 0
---                             if f == 1
---                             then loop (c + (if pc .&. 1 == 0 then flips else -flips))
---                                         (max m flips)
---                                         (pc+1)
---                             else do
---                                 VG.reverse $ VM.unsafeSlice 0 f tperm
---                                 count_flips (flips+1)
