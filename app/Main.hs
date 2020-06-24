module Main where

import Control.Concurrent
import Control.Exception
import Control.Parallel.Strategies

import Data.Binary     (decodeFile)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Text.Printf (printf)

import KMeans

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read <$> readFile "clusters"
  let numChunks = 64
      num = length clusters
  npoints <- evaluate (length points)
  t0 <- getCurrentTime
  finalClusters <- kmeansStrat numChunks num points clusters
  printTimeSince t0
  putStrLn "final clusters:"
  putStrLn $ unlines $ map show finalClusters

-- -----------------------------------------------------------------------------
-- Split a list into a list of lists

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs
  where chunk _ [] = []
        chunk n ys = let (as, bs) = splitAt n ys in as : chunk n bs

-- -----------------------------------------------------------------------------
-- Assign points to clusters in parallel

parStepStrat :: Int -> [Cluster] -> [[Point]] -> [Cluster]
parStepStrat num clusters points =
  makeNewClusters $
    foldr1 combinePointSums (map (assign num clusters) points `using` parList rseq)

-- -----------------------------------------------------------------------------
-- Parallel K-Means algorithm. Assign points to clusters until a maximum
-- number of iterations occur, or the clusters converge.

maxLoops :: Int
maxLoops = 80

kmeansStrat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansStrat numChunks num points = loop 0 where
  chunks = split numChunks points
  loop i clusters | i > maxLoops =
    return clusters
  loop i clusters = do
    let clusters' = parStepStrat num clusters chunks
    if clusters' == clusters
      then return clusters else loop (i+1) clusters'

-- -----------------------------------------------------------------------------
-- Print a time difference
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
