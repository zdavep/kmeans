module Main where

import Control.Concurrent
import Control.Exception
import Control.Parallel.Strategies

import Data.Binary     (decodeFile)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Debug.Trace     (trace)
import Text.Printf     (printf)

import KMeans

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read <$> readFile "clusters"
  t0 <- getCurrentTime
  finalClusters <- kmeansStrat 64 points clusters
  printTimeSince t0
  putStrLn "final clusters:"
  putStrLn $ unlines $ map show finalClusters

-- -----------------------------------------------------------------------------
-- Split a list into a list of lists

split :: Int -> [a] -> [[a]]
split n xs = chunk (length xs `quot` n) xs
  where chunk _ [] = []
        chunk n ys = let (as, bs) = splitAt n ys in as : chunk n bs

-- -----------------------------------------------------------------------------
-- Parallel K-Means algorithm. Assign points to clusters until a maximum
-- number of iterations (100) occur, or the clusters converge.

kmeansStrat :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansStrat n points = loop 0 where
  chunks = split n points
  loop i _ | trace ("i = " <> show i) False = undefined
  loop i c | i > 100 = return c
  loop i c = do
    let c' = parStepStrat c chunks
    if c' == c then return c else loop (i+1) c'

-- -----------------------------------------------------------------------------
-- Assign points to clusters in parallel

parStepStrat :: [Cluster] -> [[Point]] -> [Cluster]
parStepStrat clusters points =
  makeNewClusters $
    foldr1 combinePointSums (map assignPoints points `using` parList rseq)
  where
    assignPoints = assign clusters

-- -----------------------------------------------------------------------------
-- Print a time difference
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
