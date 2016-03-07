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
import Control.Monad

import Debug.Trace

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Unboxed as V

main = do
    n <- getArgs >>= readIO.head

    let
        factorialTable :: V.Vector Int
        factorialTable = V.constructN n (\v -> if V.null v then 1 else (V.last v) * (V.length v + 1))

    --- putStrLn $ show $ V.toList factorialTable

    --- let (maxFlipCount, checkSum) = fannkuchNaïve n
    --- putStr $ unlines
    ---     [ show checkSum
    ---     , "Pfannkuchen(" ++ show n ++ ") = " ++ show maxFlipCount
    ---     ]

    (checkSum, maxFlipsCount) <- fannkuch n
    --- printf "%d\nPfannkuchen(%d) = %d\n" checksum n maxflips
    putStr $ unlines
        [ show checkSum
        , "Pfannkuchen(" ++ show n ++ ") = " ++ show maxFlipsCount
        ]

fannkuch :: Int -> IO (Int, Int)
fannkuch n = do

    -- Mutable VM.IOVector Int vectors of length n

    -- permutation vector
    !perm  <- V.unsafeThaw $ V.enumFromN 1 n

    -- temporary copy of perm
    !tperm <- VM.new n

    -- To optimize the process I use an intermediate data structure, count[],
    -- which keeps count of how many rotations have been done at every level.
    -- Apparently, count[0] is always 0, as there is only one element at that
    -- level, which can't be rotated; count[1] = 0..1 for two elements,
    -- count[2] = 0..2 for three elements, etc.
    !count <- VM.replicate n 0

    let
        fannkuchLoop
            :: Int -- ^ c, the checkSum
            -> Int -- ^ m, the maxFlipsCount
            -> Int -- ^ pc, the permutation index.
            -> IO (Int, Int)
                -- ^ Returns (checkSum, maxFlipsCount)
        fannkuchLoop !c !m !pc = do
            morePerms <- permNext perm n count
            --- perm' <- V.freeze perm
            --- traceIO $ show $ V.toList perm'
            putStr "permIndex "
            putStr $ show pc
            putStr " perm "
            putStr =<< showVec perm
            putStr " count "
            putStr =<< showVec count

            permGen <- VM.new n
            countGen <- VM.new n
            permIndex n pc permGen countGen

            putStr " permGen "
            putStr =<< showVec permGen
            putStr " countGen "
            putStr =<< showVec countGen
            putStrLn ""

            if morePerms == False
            then return (c,m)
            else do
                VM.unsafeCopy tperm perm
                let countFlips !flips = {-# SCC "countFlips" #-} do
                        tperm0 <- VM.unsafeRead tperm 0
                        if tperm0 == 1
                        then
                            fannkuchLoop
                                (c + (if pc .&. 1 == 0 then flips else -flips))
                                (max m flips)
                                (pc+1)
                        else do
                            VG.reverse $ VM.unsafeSlice 0 tperm0 tperm
                            countFlips (flips+1)
                countFlips 0

    fannkuchLoop 0 0 1


showVec :: VM.IOVector Int -> IO String
--- showVec x = do
---     x' <- V.freeze x
---     return $ show $ V.toList x'
--- showVec x = V.freeze x >>= return . show . V.toList
showVec x = return . show . V.toList =<< V.freeze x

-- | Generate the next permutation from the count array.
permNext
    :: VM.IOVector Int
        -- ^ Vector to be mutated to next permuation.
    -> Int
        -- ^ Length of permutation.
    -> VM.IOVector Int
        -- ^ count vector for recursion depth state.
    -> IO Bool
        -- ^ Returns False when there are no more permutations.
permNext perm !n !count = permNextLoop 1
  where
    permNextLoop
        :: Int -- i loops over [1..n-1]
        -> IO Bool
            -- ^ Returns False when there are no more permutations.
    permNextLoop !i
        | i >= n = return False
        | otherwise = do

            --- putStrLn . (\x -> "before rotate by " ++ show i ++ " " ++ x) =<< showVec perm


            --- perm0 <- VM.unsafeRead perm 0 -- perm0 is the 0th value in perm
            --- let
            ---     rotate -- left-rotate the first (i+1) places of perm.
            ---         :: Int
            ---         -> IO ()
            ---     rotate j
            ---         | j >= i = VM.unsafeWrite perm i perm0
            ---         | otherwise = do
            ---             !permj1 <- VM.unsafeRead perm (j+1)
            ---             VM.unsafeWrite perm j permj1
            ---             rotate (j+1)
            --- rotate 0 -- left-rotate the first i+1 places of perm

            -- left-rotate the first i+1 places of perm
            rotate' (i+1) perm

            --- putStrLn . ("after  rotate      " ++) =<< showVec perm

            counti <- VM.unsafeRead count i
            if counti >= i
            then do
                VM.unsafeWrite count i 0
                permNextLoop (i+1)
            else do
                VM.unsafeWrite count i (counti+1)
                return True

rotate' -- left-rotate the first i places of perm where i >= 2.
    :: Int -- ^ must be >= 2
    -> VM.IOVector Int
    -> IO ()
--- rotate' 1 _ = return () -- TODO do we really need this case?
rotate' i perm = do -- TODO memmove?
    perm0 <- VM.unsafeRead perm 0
    forM_ [0..i-2] $ \j -> do
        permj <- VM.unsafeRead perm $ j+1
        VM.unsafeWrite perm j permj
    VM.unsafeWrite perm (i-1) perm0


-- | Generate permutation from a permuation index.
--
-- > It should be clear now how to generate a permutation and corresponding
-- > count[] array from an arbitrary index. Basically,
-- >
-- > count[k] = ( index % (k+1)! ) / k!
-- >
-- > is the number of rotations we need to perform on elements 0..k.
-- > Doing it in the descending order from n-1 to 1 gives us both the count[]
-- > array and the permutation.
permIndex
    :: Int -- ^ Length of permuation.
    -> Int -- ^ ith permutation index.
    -> VM.IOVector Int -- ^ Mutable permutation output.
    -> VM.IOVector Int -- ^ Mutable count vector output.
    -> IO ()
--- permIndex !n !i !perm !count = undefined
permIndex !n !i !perm !count = do

    -- initialize perm to [1,2,..n]
    forM_ [0..n-1] (\k -> VM.unsafeWrite perm k $ k + 1)

    -- count[0] is always 0. zero-initialization is done by VM.new.
    -- VM.unsafeWrite count 0 0

    -- forM_ k = [n-1..1] over the count vector
    forM_ (take (n-1) $ iterate (subtract 1) (n-1)) $ \ k -> do
        let countk = (i `mod` factorial (k+1)) `div` factorial k
        VM.unsafeWrite count k countk
        replicateM_ countk $ rotate' (k+1) perm

  where
    factorial 1 = 1 -- TODO factorialTable
    factorial z = z * factorial (z-1)




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



--- -- this is the special permuation ordering necessary for the permutation indices to have the required evenness.
--- permute :: [Int] -> [[Int]]
--- permute [] = [[]]
--- permute [x] = [[x]]
--- permute xs = take (length xs) $ iterate rotateLeft xs
---   where
---     rotateLeft (x:xs) = xs ++ [x]
---     -- subRotate (x:xs) = x : rotateLeft xs
---     -- subRotate (x:xs) =


-- fannkuchNaïve doesn't work because the permutation order is different than the
-- reference implementation, so the evenness assigned to each permutation is
-- wrong and the checksum comes out wrong.
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
        go (1:_) !cnt = cnt
        go xs@(x:_) !cnt = go (reverse lxs ++ rxs) (cnt + 1)
          where
            (lxs, rxs) = splitAt x xs

