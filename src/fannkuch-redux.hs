{-  The Computer Language Benchmarks Game

    http://benchmarksgame.alioth.debian.org/

    Contributed by Branimir Maksimovic.
    Optimized/rewritten by Bryan O'Sullivan.
    Modified by Gabriel Gonzalez.
    Parallelized and rewritten by James Brock 2016.

    This fannkuch-redux Haskell implementation uses mutable data vectors
    for performance.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
--- {-# LANGAUGE NamedFieldPuns #-}

import System.Environment
import Text.Printf
import Data.Bits
import Data.Monoid
import Data.List
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

import Debug.Trace

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Unboxed as V

--- factorialTable :: V.Vector Int
--- factorialTable = V.empty
---
--- factorialTable' :: VM.IOVector Int
--- -- factorialTable' = VM.new 0


-- | Data for a slice of fannkuch-redux calculation work on a permutation
-- index range.
data FWork = FWork
    { factorialTable :: !(V.Vector Int)
        -- ^ Local factorial function lookup table.
    , permIndexBegin :: !Int
        -- ^ Lower bound inclusive of the permutation index for this work.
    , permIndexEnd   :: !Int
        -- ^ Upper bound exclusive of the permutation index for this work.
    , perm           :: !(VM.IOVector Int)
        -- ^ Permutation vector.
    , tperm          :: !(VM.IOVector Int)
        -- ^ Working temporary permutation vector.
    , count          :: !(VM.IOVector Int)
        -- ^ Count vector.
        --
        -- > To optimize the process I use an intermediate data structure,
        -- > count[], which keeps count of how many rotations have been done
        -- > at every level. Apparently, count[0] is always 0, as there is
        -- > only one element at that level, which can't be rotated;
        -- > count[1] = 0..1 for two elements,
        -- > count[2] = 0..2 for three elements, etc.
    }

-- | Construct a slice of fannkuch-redux calculation work.
mkFWork
    :: Int
        -- ^ n, the size of the total fannkuch-redux problem.
    -> Int
        -- ^ Lower bound inclusive of the permutation index for this work.
    -> Int
        -- ^ Upper bound exclusive of the permutation index for this work.
    -> IO FWork
mkFWork n pBegin pEnd = do

    perm' <- VM.new n
    tperm' <- VM.new n
    count' <- VM.new n

    permIndex n pBegin perm' count'

    return $ FWork
        --- { factorialTable = V.constructN n (\v -> if V.null v then 1 else (V.last v) * (V.length v + 1))
        { factorialTable = V.generate n $ factorial . (+1)
        , permIndexBegin = pBegin
        , permIndexEnd   = pEnd
        , perm           = perm'
        , tperm          = tperm'
        , count          = count'
        }

-- | Evaluate factorial function from the lookup table.
factorialEval :: V.Vector Int -> Int -> Int
factorialEval lookupTable x = V.unsafeIndex lookupTable $ x-1

-- | work function with tail-recursion and mutable state.
work
    :: FWork
        -- ^ Slice of fannkuch-redux to be calculated.
    --- -> Int
    ---     -- ^ Current permutation index, 1-based
    --- -> Int
    ---     -- ^ Current max flips count
    --- -> Int
    ---     -- ^ Current checksum
    -> IO (Int, Int)
        -- ^ (max flips count, checksum)
work FWork{..} = do
    let
        go
            :: Int
                -- ^ Current permutation index, 1-based
            -> Int
                -- ^ Current max flips count
            -> Int
                -- ^ Current checksum
            -> IO (Int, Int)
                -- ^ (max flips count, checksum)
        go pindex maxFlipCount checkSum
            | pindex == permIndexEnd   = return (maxFlipCount, checkSum)
            | otherwise                = do
                --- _ <- permNext perm (VM.length perm) count
                VM.unsafeCopy tperm perm
                let countFlips !flips = {-# SCC "countFlips" #-} do
                        tperm0 <- VM.unsafeRead tperm 0
                        if tperm0 == 1
                        then do
                            _ <- permNext perm (VM.length perm) count
                            go -- TODO pull this out of countFlips
                                (pindex+1)
                                (max maxFlipCount flips)
                                (checkSum + (if pindex .&. 1 == 0 then flips else -flips))
                        else do
                            VG.reverse $ VM.unsafeSlice 0 tperm0 tperm
                            countFlips (flips+1)
                countFlips 0

    -- go 0 0 1
    go permIndexBegin 0 0




fannkuch'
    :: Int
        -- ^ n, the size of the fannkuch-redux problem.
    -> IO (Int, Int)
        -- ^ (max flips count, checksum)
fannkuch' n = do

    -- number of permutations to consider
    let numPermutations = factorial n

    -- number of cores available for work.
    numCapabilities <- getNumCapabilities

    -- the smallest unit of work which is worth forking.
    let workSizeMin = 1000

    -- the amount of work which we would like to give to each core.
    let workSize = max workSizeMin $ numPermutations `div` numCapabilities

    -- divide up the permutations into workSize units
    let workBoundaries = takeWhile (<=numPermutations) $ iterate (+workSize) 1

    -- upper and lower permutation index bounds for each workSize unit
    let workRanges = zip workBoundaries $ tail workBoundaries ++ [numPermutations+1]

    traceIO $ "numPermutations " ++ show numPermutations
    traceIO $ "numCapabilities " ++ show numCapabilities
    traceIO $ "workRanges " ++ show workRanges

    -- get ready to perform the work
    works <- sequence $ fmap (\(b,e) -> mkFWork n b e) workRanges

    -- perform the work
    --- worked <- sequence $ fmap work works
    worked <- mapConcurrently work works

    -- gather up the results and return
    return $ foldl1' (\(x0,y0) (x1,y1) -> (max x0 x1,y0+y1)) worked




main = do
    n <- getArgs >>= readIO . head

    (maxFlipsCount', checkSum') <- fannkuch' n
    putStr $ unlines
        [ show checkSum'
        , "Pfannkuchen'(" ++ show n ++ ") = " ++ show maxFlipsCount'
        ]

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

            --- putStr "permIndex "
            --- putStr $ show pc
            --- putStr " perm "
            --- putStr =<< showVec perm
            --- putStr " count "
            --- putStr =<< showVec count

            --- permGen <- VM.new n
            --- countGen <- VM.new n
            --- permIndex n pc permGen countGen

            --- putStr " permGen "
            --- putStr =<< showVec permGen
            --- putStr " countGen "
            --- putStr =<< showVec countGen
            --- putStrLn ""

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
showVec x = return . show . V.toList =<< V.freeze x

-- | Generate the next permutation from the count array.
permNext -- TODO make a version that returns ()
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


-- | From a permutation index, generate permutation and count.
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
    :: Int -- ^ Length of permutation.
    -> Int -- ^ ith permutation index.
    -> VM.IOVector Int -- ^ Mutable permutation vector output.
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


factorial :: Int -> Int
factorial z0 = go z0 1
  where
    go 1 answer = answer
    go z answer = go (z-1) (answer*z)




--- -- http://en.literateprograms.org/Kth_permutation_%28Haskell%29
---
--- -- radix representation
--- rr :: Int -> Int -> [Int]
--- rr 0 _ = []
--- rr n k = k `mod` n : rr (n-1) (k `div` n)
--- --- rr n k = rr (n-1) (k `div` n) <> [k `mod` n]
---
--- -- direct representation from radix rep
--- dfr :: [Int] -> [Int]
--- dfr = foldr (\x rs -> x : [r + (if x <= r then 1 else 0) | r <- rs]) []
---
--- -- generate permutation
--- perm :: [a] -> Int -> [a]
--- perm xs k = [xs !! i | i <- dfr (rr (length xs) k)]



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

