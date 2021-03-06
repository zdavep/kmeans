module KMeans where

import Control.DeepSeq

import Data.Binary
import Data.Foldable (minimumBy)
import Data.Function

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

import GHC.Generics

-- Point is a X and Y coordinate pair.
data Point =
  Point {-# UNPACK #-} !Double -- X coordinate
        {-# UNPACK #-} !Double -- y coordinate
  deriving
    (Eq, Ord, Read, Show, Generic)

-- Forces full evaluation of Point to normal form.
instance NFData Point

-- Allows for points to be read from file in binary format.
instance Binary Point where
  put (Point x y) = put x >> put y
  get = do x <- get; Point x <$> get

-- Distance function
sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1 - y2)^2)

-- Reads all points from a binary file and unpacks them to a list.
readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Point (read (B.unpack xs)) (read (B.unpack ys)) | (xs:ys:_) <- ls ]
  return points

-- PointSum is a summation of a group of points.
data PointSum =
  PointSum !Int     -- Number of points
           !Double  -- Sum of X coordinates
           !Double  -- Sum of Y coordinates
  deriving (Eq, Show)

-- Combine two point sums.
instance Semigroup PointSum where
  (PointSum c1 x1 y1) <> (PointSum c2 x2 y2) = PointSum (c1+c2) (x1+x2) (y1+y2)

-- Add a point to a point sum.
addPointToSum :: PointSum -> Point -> PointSum
addPointToSum (PointSum count xs ys) (Point x y) =
  PointSum (count + 1) (xs + x) (ys + y)

-- Calculate the cluster center and return a new cluster with the given ID.
pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster clusterId (PointSum c xs ys) =
    Cluster clusterId (Point x y)
  where
    x = xs / fromIntegral c
    y = ys / fromIntegral c

-- Combine two point sum vectors.
combinePointSums :: V.Vector PointSum -> V.Vector PointSum -> V.Vector PointSum
combinePointSums = V.zipWith (<>)

-- Cluster defines the central point in a group of points.
data Cluster = Cluster
  { clId   :: {-# UNPACK #-} !Int   -- Cluster identifier
  , clCent :: {-# UNPACK #-} !Point -- Central point in the cluster
  }
  deriving (Eq, Ord, Read, Show, Generic)

-- Forces full evaluation of Cluster to normal form.
instance NFData Cluster

-- Convert a vector of point sums to a list of clusters.
makeNewClusters :: V.Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster cid ps
  | (cid, ps@(PointSum count _ _)) <- zip [0..] (V.toList vec)
  , count > 0
  ]

-- Assign points to clusters producing a new point sum vector.
assign :: [Cluster] -> [Point] -> V.Vector PointSum
assign clusters points = V.create $ do
  vec <- MV.replicate (length clusters) (PointSum 0 0 0)
  let
    addpoint p = do
      let c = nearest p; cid = clId c
      ps <- MV.read vec cid
      MV.write vec cid $! addPointToSum ps p
  mapM_ addpoint points
  return vec
  where
    distances p = [(c, sqDistance (clCent c) p) | c <- clusters]
    nearest p = fst $ minimumBy (compare `on` snd) (distances p)
