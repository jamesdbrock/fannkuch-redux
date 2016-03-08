{-  The Computer Language Benchmarks Game

    http://benchmarksgame.alioth.debian.org/

    Contributed by Branimir Maksimovic.
    Optimized/rewritten by Bryan O'Sullivan.
    Modified by Gabriel Gonzalez.
    Parallelized and rewritten by James Brock 2016.

    This fannkuch-redux Haskell implementation uses mutable data vectors
    for performance.
-}

{-
jdb@hotchip:~/work/fannkuch-redux$ bin/fannkuch-redux-exe 12 +RTS -s -N4
3968050
Pfannkuchen(12) = 65
  34,002,092,608 bytes allocated in the heap
      32,506,128 bytes copied during GC
          89,976 bytes maximum residency (2 sample(s))
          49,288 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     20281 colls, 20281 par    0.344s   0.147s     0.0000s    0.0006s
  Gen  1         2 colls,     1 par    0.001s   0.000s     0.0001s    0.0002s

  Parallel GC work balance: 24.19% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.004s  (  0.003s elapsed)
  MUT     time   60.612s  ( 15.902s elapsed)
  GC      time    0.345s  (  0.147s elapsed)
  EXIT    time    0.001s  (  0.000s elapsed)
  Total   time   60.964s  ( 16.053s elapsed)

  Alloc rate    560,981,939 bytes per MUT second

  Productivity  99.4% of total user, 377.6% of total elapsed

gc_alloc_block_sync: 29203
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 2

jdb@hotchip:~/work/fannkuch-redux$ bin/fannkuch-redux-exe 12 +RTS -s -N1
3968050
Pfannkuchen(12) = 65
  34,002,032,576 bytes allocated in the heap
      35,574,208 bytes copied during GC
          56,720 bytes maximum residency (2 sample(s))
          25,200 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     65223 colls,     0 par    0.107s   0.252s     0.0000s    0.0001s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.001s elapsed)
  MUT     time   57.289s  ( 57.220s elapsed)
  GC      time    0.107s  (  0.252s elapsed)
  EXIT    time    0.001s  (  0.000s elapsed)
  Total   time   57.397s  ( 57.473s elapsed)

  Alloc rate    593,513,991 bytes per MUT second

  Productivity  99.8% of total user, 99.7% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main(main) where

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


main = do
    n <- readIO . head =<< getArgs

    (maxFlipsCount, checkSum) <- fannkuch n

    putStr $ unlines
        [ show checkSum
        , "Pfannkuchen(" ++ show n ++ ") = " ++ show maxFlipsCount
        ]


-- showVec :: VM.IOVector Int -> IO String
-- showVec x = return . show . V.toList =<< V.freeze x

factorial :: Int -> Int
factorial z0 = go z0 1
  where
    go 1 !answer = answer
    go !z !answer = go (z-1) (answer*z)


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
mkFWork !n !pBegin !pEnd = do

    perm'  <- VM.new n
    tperm' <- VM.new n
    count' <- VM.new n

    permIndex n pBegin perm' count'

    return $ FWork
        { factorialTable = V.generate n $ factorial . (+1)
        , permIndexBegin = pBegin
        , permIndexEnd   = pEnd
        , perm           = perm'
        , tperm          = tperm'
        , count          = count'
        }

-- | Evaluate factorial function from the lookup table.
factorialEval :: V.Vector Int -> Int -> Int
factorialEval lookupTable !x = V.unsafeIndex lookupTable $ x-1

-- | work function with tail-recursion and mutable state.
work
    :: FWork
        -- ^ Slice of fannkuch-redux to be calculated.
    -> IO (Int, Int)
        -- ^ (max flips count, checksum)
work !FWork{..} = go permIndexBegin 0 0
  where
    go
        :: Int
            -- ^ Current permutation index, 1-based
        -> Int
            -- ^ Current max flips count
        -> Int
            -- ^ Current checksum
        -> IO (Int, Int)
            -- ^ (max flips count, checksum)
    go !pindex !maxFlipCount !checkSum
        | pindex == permIndexEnd   = return (maxFlipCount, checkSum)
        | otherwise                = do
            flips <- countFlips
            permNext (VM.length perm) perm count -- TODO get rid of length
            go
                (pindex+1)
                (max maxFlipCount flips)
                (checkSum + (if pindex .&. 1 == 0 then flips else -flips))

    countFlips :: IO Int
    countFlips = do
        VM.unsafeCopy tperm perm
        goFlips 0
      where
        goFlips :: Int -> IO Int
        goFlips !flips = do
            tperm0 <- VM.unsafeRead tperm 0
            if tperm0 == 1
            then return flips
            else do
                VG.reverse $ VM.unsafeSlice 0 tperm0 tperm
                goFlips (flips+1)


fannkuch
    :: Int
        -- ^ n, the size of the fannkuch-redux problem.
    -> IO (Int, Int)
        -- ^ (max flips count, checksum)
fannkuch !n = do

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

    -- get ready to perform the work
    works <- sequence $ fmap (\(b,e) -> mkFWork n b e) workRanges

    -- perform the work
    worked <- mapConcurrently work works

    -- gather up the results and return
    return $ foldl1' (\(x0,y0) (x1,y1) -> (max x0 x1,y0+y1)) worked



-- | Generate the next permutation from the count array.
permNext -- TODO make a version that returns (), and uses the factorialTable
    :: Int
        -- ^ Length of permutation.
    -> VM.IOVector Int
        -- ^ Vector to be mutated to next permuation.
    -> VM.IOVector Int
        -- ^ count vector for recursion depth state.
    -> IO ()
permNext !n !perm !count = go 1
  where
    go
        :: Int -- i loops over [1..n-1]
        -> IO ()
    go !i
        | i >= n    = return ()
        | otherwise = do

            -- left-rotate the first i+1 places of perm
            rotate' perm $ i+1

            counti <- VM.unsafeRead count i
            if counti >= i
            then do
                VM.unsafeWrite count i 0
                go $ i+1
            else do
                VM.unsafeWrite count i $ counti+1
                return ()

-- | Left-rotate the first i places of perm where i >= 2.
rotate'
    :: VM.IOVector Int
    -> Int
        -- ^ must be >= 2
    -> IO ()
rotate' !perm !i = do -- TODO memmove?
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
permIndex !n !i !perm !count = do

    -- initialize perm to [1,2,..n]
    forM_ [0..n-1] (\k -> VM.unsafeWrite perm k $ k + 1)

    -- count[0] is always 0. zero-initialization is done by VM.new.
    -- VM.unsafeWrite count 0 0

    -- forM_ k = [n-1..1] over the count vector
    forM_ (take (n-1) $ iterate (subtract 1) (n-1)) $ \ k -> do
        let countk = (i `mod` factorial (k+1)) `div` factorial k
        VM.unsafeWrite count k countk
        replicateM_ countk $ rotate' perm $ k+1



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

